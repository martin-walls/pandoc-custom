-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt
  --
-- S matches any character in the string
-- P matches string literally
-- x + y matches either x or y
-- x - y matches x if y doesn't match
-- #x matches x but doesn't consume input

local whitespacechar = S(" \t\r\n")
-- local specialchar = S("/*~[]\\{}|")
local specialchar = S("[]|`*~=")
local wordchar = (1 - (whitespacechar + specialchar))
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n" -- at most one \r, followed by \n
local blankline  = spacechar^0 * newline -- at least 0 spaces followed by a newline
local endline = newline * #-blankline
local cellsep = spacechar^0 * P"|"

local function trim(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- local function ListItem(indent, startchars)
--   local start
--   if char == nul then
--     start = S"*#"
--   else
--     start = S(startchars)
--   end
--   local subitem = function(c)
--     return ListItem(indent + 2, c)
--   end
--   local parser = spacechar^indent
--                * start
--                * spacechar^1
--                * Ct((V"Inline" - (newline * spacechar^indent * S"*#"))^0)
--                * newline
--   return parser
-- end

-- GRAMMAR
G = P{ "Doc",
  Doc = Ct(V"Frontmatter"^-1 * V"Block"^0)
      / pandoc.Pandoc ;
  -- Ignore frontmatter in output
  Frontmatter = P"---"
              * newline
              * C((1 - (newline * P"---"))^0)
              * newline
              * P"---"
              / pandoc.Null ;
  -- blocks
  Block = blankline^0
        * ( V"Header"
          + V"CodeBlock"
          -- + V"List"
          + V"Para" ) ;
  -- paragraph
  Para = Ct(V"Inline"^1)
       * newline
       / pandoc.Para ;
  -- header
  Header = (P("#")^1 / string.len)
         * spacechar^1
         * Ct((V"Inline"))
         / pandoc.Header ;
  -- List = V"BulletList" ;
  -- BulletList = Ct(ListItem(0,"-*")^1)
  --            / pandoc.BulletList ;
  -- inline elements
  Inline = V"Emph"
         + V"Strong"
         + V"Strikeout"
         + V"Highlight"
         + V"Link"
         + V"Str"
         + V"Space"
         + V"CodeInline" ;
  -- strings
  Str = wordchar^1
      / pandoc.Str ;
  -- spaces
  Space = spacechar^1
        / pandoc.Space ;
  -- links to other files
  Link = P"[["
       * C((1 - (P"]]" + P"|"))^0)
       * (P"|" * Ct((V"Inline" - P"]]")^1))^-1 * P"]]"
       / function(url, desc)
          local txt = desc or {pandoc.Str(url)}
          return pandoc.Link(txt, url)
        end ;
  -- `inline code`
  CodeInline = P"`"
       * C((1 - P"`")^0)
       * P"`"
       / trim / pandoc.Code ;
  -- ```code block```
  CodeBlock = (P"```" * wordchar^0)
            * newline
            * C((1 - (newline * P"```"))^0)
            * newline
            * P"```"
            / pandoc.CodeBlock ; -- TODO make syntax highlighting work
  -- **bold**
  Strong = P"**"
         * Ct((V"Inline" - P"**")^1)
         * P"**"
         / pandoc.Strong ;
  -- *italic*
  Emph = P"*"
       * Ct((V"Inline" - P"*")^1)
       * P"*"
       / pandoc.Emph ;
  -- ~~strikethrough~~
  Strikeout = P"~~"
            * Ct((V"Inline" - P"~~")^1)
            * P"~~"
            / pandoc.Strikeout ;
  -- ==highlight== ; currently mapping this to underline
  Highlight = P"=="
            * Ct((V"Inline" - P"==")^1)
            * P"=="
            / pandoc.Underline ;
}



function Reader(input)
  return lpeg.match(G, tostring(input))
end