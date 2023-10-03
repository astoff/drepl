local json = require 'dkjson'
local repl = require 'repl'
local stdout = io.stdout
local stdin = io.stdin
local print = print
local format = string.format

local drepl = repl:clone()
drepl.keep_running = true
drepl:loadplugin 'autoreturn'
drepl:loadplugin 'completion'
drepl._features.console = true -- Lie about this to make pretty_print happy.
drepl:loadplugin 'pretty_print'

-- function drepl:displayresults(results)
--   if results.n == 0 then return end
--   print(compat.unpack(results, 1, results.n))
-- end

function drepl:displayerror(err)
  print(err)
end

local function reply(data)
  stdout:write(format("\x1b]5161;%s\x1b\\", json.encode(data)))
  stdout:flush()
end

function drepl:showprompt(prompt)
  stdout:write(prompt .. ' ')
  stdout:flush()
end

function drepl:process_line()
  reply{op="status", status="ready"}
  local line = stdin:read()
  if line == nil then -- Got an EOF signal
    self.keep_running = false
    return
  end
  if line:sub(1, 2) ~= "\x1b%" then
    error(format("Unexpected input: %q", line))
  end
  local message = json.decode(line, 3)
  local method = self["drepl_" .. message.op]
  if not method then
    error(format("Unknown op: %s", message.op))
  end
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
  reply{op="status", status="ready"}
  reply{
    id=args.id,
    status=not err and "complete" or cont and "incomplete" or "invalid",
    prompt=err and self:getprompt(2) .. " " or nil,
    indent=""
  }
end

function drepl:drepl_complete(args)
  local prefix = args.code:sub(1, args.offset)
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
  reply{
    id=args.id,
    candidates=cands
  }
end

function drepl:main()
  self:prompt(1)
  while self.keep_running do
    local ok, err = pcall(self.process_line, self)
    if not ok then reply{op="log", text=err} end
  end
end

return drepl
