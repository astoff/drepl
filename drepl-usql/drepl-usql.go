package main

import (
	"bufio"
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"os/user"
	"regexp"

	"github.com/gohxs/readline"
	"github.com/xo/usql/env"
	"github.com/xo/usql/handler"
	"github.com/xo/usql/rline"
	"github.com/xo/usql/text"

	// TODO: Make selected drivers customizable
	_ "github.com/xo/usql/drivers/clickhouse"
	_ "github.com/xo/usql/drivers/csvq"
	_ "github.com/xo/usql/drivers/mysql"
	_ "github.com/xo/usql/drivers/oracle"
	_ "github.com/xo/usql/drivers/postgres"
	_ "github.com/xo/usql/drivers/sqlite3"
	_ "github.com/xo/usql/drivers/sqlserver"
)

// Implements usql's rline.IO interface
type Dline struct {
	scanner   *bufio.Scanner
	handler   *handler.Handler
	completer readline.AutoCompleter
}

// One-size-fits-all struct to decode an incoming message
type InMsg struct {
	Id   int
	Op   string
	Code string
	Pos  int
}

// ReadMsg blocks and reads a framed message from the editor
func (l *Dline) ReadMsg() (*InMsg, error) {
	if err := l.SendStatus("ready"); err != nil {
		return nil, err
	}
	var b []byte
	for {
		ok := l.scanner.Scan()
		if ok != true {
			err := l.scanner.Err()
			if err == nil {
				err = io.EOF
			}
			return nil, err
		}
		s := l.scanner.Bytes()
		if bytes.HasPrefix(s, []byte("\033+")) {
			b = append(b, []byte(s)[2:]...)
			continue
		} else if bytes.HasPrefix(s, []byte("\033=")) {
			b = append(b, []byte(s)[2:]...)
			break
		}
		return nil, fmt.Errorf("invalid input")
	}
	var msg InMsg
	err := json.Unmarshal(b, &msg)
	if err != nil {
		return nil, err
	}
	return &msg, nil
}

// SendMsg sends a framed message to the editor
func (l *Dline) SendMsg(msg map[string]any) error {
	s, err := json.Marshal(msg)
	if err != nil {
		return err
	}
	_, err = fmt.Fprintf(l.Stdout(), "\033]5161;%s\033\\", s)
	return err
}

func (l *Dline) SendLogo() error {
	msg := "\033]5151;{\"type\":\"image/png\"}\n%s\033\\\n"
	s := base64.StdEncoding.EncodeToString(text.LogoPng)
	_, err := fmt.Fprintf(l.Stdout(), msg, s)
	return err
}

func (l *Dline) SendStatus(status string) error {
	return l.SendMsg(map[string]any{"op": "status", "status": status})
}

func (l *Dline) SendLog(format string, a ...any) error {
	text := fmt.Sprintf(format, a...)
	return l.SendMsg(map[string]any{"op": "log", "text": text})
}

func (l *Dline) MakePrompt() string {
	return l.handler.Prompt(env.Get("PROMPT1"))
}

var isCompleteRE = regexp.MustCompile(
	fmt.Sprintf(
		`(?i)^\s*(|%s|%s|%s)\s*$`,
		text.HelpPrefix, text.QuitPrefix, text.ExitPrefix),
)

func (l *Dline) CheckInput(code string) (status string, prompt string) {
	if m := isCompleteRE.FindStringIndex(code); m != nil {
		return "complete", ""
	}
	h := l.handler
	buf := h.Buf()
	buf.Reset([]rune(code))
	cmd, _, err := buf.Next(env.Unquote(h.User(), false, env.All()))
	ready := buf.Ready()
	prompt = l.MakePrompt()
	buf.Reset(nil)
	if err != nil {
		status = "invalid"
	} else if cmd == "" && !ready {
		status = "incomplete"
	} else {
		status = "complete"
	}
	return status, prompt
}

func (l *Dline) Complete(code string, pos int) (string, []string) {
	if l.completer == nil || len(code) < pos {
		return "", nil
	}
	coder := []rune(code)
	candr, i := l.completer.Do(coder, pos)
	start := pos - i
	if start < 0 {
		return "", nil
	}
	prefix := coder[start:pos]
	cands := make([]string, 0, len(candr))
	for _, c := range candr {
		c = append(prefix, c...)
		cands = append(cands, string(c))
	}
	return string(prefix), cands
}

// Next returns the next line of runes (excluding '\n') from the input.
func (l *Dline) Next() ([]rune, error) {
	fmt.Print(l.MakePrompt())
	for {
		msg, err := l.ReadMsg()
		if err == io.EOF {
			os.Exit(0)
		}
		if err != nil {
			fmt.Println("error:", err.Error())
			return nil, nil
		}
		switch msg.Op {
		case "eval":
			l.SendMsg(map[string]any{"id": msg.Id})
			l.SendStatus("rawio")
			return []rune(msg.Code), nil
		case "checkinput":
			status, prompt := l.CheckInput(msg.Code)
			l.SendMsg(map[string]any{
				"id":     msg.Id,
				"status": status,
				"prompt": prompt,
			})
		case "complete":
			prefix, cands := l.Complete(msg.Code, msg.Pos)
			l.SendMsg(map[string]any{
				"id":         msg.Id,
				"prefix":     prefix,
				"candidates": cands,
			})
		default:
			l.SendMsg(map[string]any{"id": msg.Id})
		}
	}
}

// Close closes the IO.
func (l *Dline) Close() error { return nil }

// Stdout is the IO's standard out.
func (l *Dline) Stdout() io.Writer { return os.Stdout }

// Stderr is the IO's standard error out.
func (l *Dline) Stderr() io.Writer { return os.Stderr }

// Interactive determines if the IO is an interactive terminal.
func (l *Dline) Interactive() bool { return true }

// Cygwin determines if the IO is a Cygwin interactive terminal.
func (l *Dline) Cygwin() bool { return false }

// Prompt sets the prompt for the next interactive line read.
func (l *Dline) Prompt(s string) {}

// Completer sets the auto-completer.
func (l *Dline) Completer(a readline.AutoCompleter) {
	l.completer = a
}

// Save saves a line of history.
func (l *Dline) Save(s string) error { return nil }

// Password prompts for a password.
func (l *Dline) Password(prompt string) (string, error) {
	fmt.Print(prompt)
	ok := l.scanner.Scan()
	if ok != true {
		return "", rline.ErrPasswordNotAvailable
	}
	return l.scanner.Text(), nil
}

// SetOutput sets the output format func.
func (l *Dline) SetOutput(f func(string) string) {}

func main() {
	usr, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	wd, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}
	env.Pset("pager", "off")
	scanner := bufio.NewScanner(os.Stdin)
	l := &Dline{scanner: scanner, completer: nil}
	l.handler = handler.New(l, usr, wd, false)
	l.handler.SetSingleLineMode(true)
	l.SendStatus("rawio") // The following line may ask for a password
	l.handler.Open(context.Background(), os.Args[1:]...)
	l.SendLogo()
	err = l.handler.Run()
	if err != nil {
		log.Fatal(err)
	}
}
