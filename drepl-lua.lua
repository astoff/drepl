local json = require 'dkjson'
local repl = require 'repl'

local concat = table.concat
local error = error
local format = string.format
local stdin, stdout = io.stdin, io.stdout

local drepl = repl:clone()
drepl.keep_running = true
drepl:loadplugin 'autoreturn'
drepl:loadplugin 'completion'
drepl._features.console = true -- Lie about this to make pretty_print happy.
drepl:loadplugin 'pretty_print'

function drepl:displayerror(err)
  stdout:write(err, "\n")
  stdout:flush()
end

local function sendmsg(data)
  stdout:write(format("\x1b]5161;%s\x1b\\", json.encode(data)))
  stdout:flush()
end

local function readmsg()
  sendmsg{op="status", status="ready"}
  local buffer, c = {}, nil
  while c ~= "=" do
    local line = stdin:read()
    if line == nil then return end
    c = line:match("^\x1b([+=])") or error(format("Unexpected input: %q", line))
    buffer[#buffer+1] = line:sub(3)
  end
  return json.decode(concat(buffer))
end

local function no_op(_, args)
  sendmsg{id=args.id}
end

function drepl:showprompt(prompt)
  stdout:write(prompt .. ' ')
  stdout:flush()
end

function drepl:process_message()
  local message = readmsg()
  if message == nil then -- Got an EOF signal
    self.keep_running = false
    return
  end
  local method = self["drepl_" .. message.op] or no_op
  method(self, message)
end

function drepl:drepl_eval(args)
  self._buffer = ""
  local v = self:handleline(args.code)
  if v == 2 then error("Incomplete input!") end
  self:prompt(1)
end

function drepl:drepl_checkinput(args)
  local _, err = self:compilechunk(args.code)
  local cont = err and self:detectcontinue(err)
  -- FIXME: the following avoids a race condition, but we should
  -- prescribe in the protocol which methods switch from ready to
  -- other states.
  sendmsg{op="status", status="ready"}
  sendmsg{
    id=args.id,
    status=not err and "complete" or cont and "incomplete" or "invalid",
    prompt=err and self:getprompt(2) .. " " or nil,
    indent=""
  }
end

function drepl:drepl_complete(args)
  local prefix = args.code:sub(1, args.pos)
  local cands = {}
  self:complete(
    prefix,
    function(line)
      local suffix = line:sub(-1, -1)
      local type = (suffix == "." and "table") or (suffix=="(" and "function") or nil
      local _, _, cand = line:find("([%a%d_]+)[.(]?$")
      cands[#cands+1] = {text=cand, type=type}
    end
  )
  sendmsg{
    id=args.id,
    candidates=cands
  }
end

function drepl:main()
  self:prompt(1)
  while self.keep_running do
    local ok, err = pcall(self.process_message, self)
    if not ok then sendmsg{op="log", text=err} end
  end
end

return drepl
