-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

local tabsize = 2

-- S matches any character in the string
-- P matches string literally
-- x + y matches either x or y
-- x - y matches x if y doesn't match
-- #x matches x but doesn't consume input

local whitespacechar = S(" \t\r\n")
-- local specialchar = S("/*~[]\\{}|")
local specialchar = S("[]|`*~=+->!")
local wordchar = (1 - (whitespacechar + specialchar))
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n" -- at most one \r, followed by \n
local blankline  = spacechar^0 * newline -- at least 0 spaces followed by a newline
-- local endline = newline * #-blankline
-- local cellsep = spacechar^0 * P"|"

local bulletliststart = S("*+-")
local orderedliststart = R"09" * P"." -- number followed by dot

local function trim(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- TODO make this work with tab indenting
-- TODO checklists
local function ListItem(level, start)
  local spaceindent = level * tabsize
  local indent = P" "^spaceindent + P"\t"^level
  local subitem = function(s)
    if level < 6 then
      return ListItem(level + 1, s)
    else
      return (1 - 1) -- fails
    end
  end
  local parser = indent
               * start
               * spacechar^1
               * Ct((V"Inline" - (newline * indent * start))^0)
               * newline
               * (Ct(subitem(bulletliststart)^1) / pandoc.BulletList
                  + Ct(subitem(orderedliststart)^1) / pandoc.OrderedList
                  + Cc(nil))
               / function (li, sublist)
                   return { pandoc.Plain(li), sublist }
                 end
  return parser
end


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
          + V"List"
          + V"QuoteBlock"
          + V"Para" ) ;
  -- paragraph
  Para = Ct(V"Inline"^1 * (V"ParaSpace" * V"Inline"^1)^0)
       * blankline
       / pandoc.Para ;
  -- used to add a space between multiple lines in the same paragraph
  -- (use a blank line in between paras)
  ParaSpace = newline / pandoc.Space ;
  -- header
  Header = (P("#")^1 / string.len)
         * spacechar^1
         * Ct((V"Inline"))
         / pandoc.Header ;
  List = V"BulletList"
       + V"OrderedList";
  BulletList = Ct(ListItem(0, bulletliststart)^1)
             / pandoc.BulletList ;
  OrderedList = Ct(ListItem(0, orderedliststart)^1)
              / pandoc.OrderedList ;
  QuoteBlock = P">"
             * spacechar^0
             * V"Para"
             / pandoc.BlockQuote ;
  -- inline elements
  Inline = V"Emph"
         + V"Strong"
         + V"Strikeout"
         + V"Highlight"
         + V"Link"
         + V"URL"
         + V"Str"
         + V"Space"
         + V"CodeInline"
         + V"SpecialChar" ;
  -- strings
  Str = wordchar^1
      / pandoc.Str ;
  -- spaces
  Space = spacechar^1
        / pandoc.Space ;
  -- if nothing else matches, just display special chars
  SpecialChar = specialchar
              / pandoc.Str ;
  -- links to other files
  Link = V"Image" + V"WikiLink"
       + V"MarkdownLink" ;
  -- Link = V"WikiLink" ;
  WikiLink = P"[["
           * C((1 - (P"]]" + P"|"))^0)
           * (P"|" * Ct((V"Inline" - P"]]")^1))^-1
           * P"]]"
           / function(url, desc)
               local txt = desc or {pandoc.Str(url)}
               return pandoc.Link(txt, url)
             end ;
  MarkdownLink = P"["
               * Ct((V"Inline" - P"]")^0)
               * P"]("
               * C((1 - P")")^0)
               * P")"
               / function(desc, url)
                   local txt = desc
                   if next(desc) == nil then
                     txt = {pandoc.Str(url)}
                   end
                   return pandoc.Link(txt, url)
                 end ;
  URL = P"http"
      * P"s"^-1
      * P":"
      * (1 - (whitespacechar + (S",.?!:;\"'" * #whitespacechar)))^1
      /  function(url)
           return pandoc.Link(pandoc.Str(url), url)
         end ;
  -- TODO handle defined image size
  -- TODO handle other embedded types eg. other pages
  Image = P"!["
        * Ct((V"Inline" - (P"]" + P"|"))^0)
        * (P"|" * Ct((V"Inline" - P"]")^1))^-1
        * P"]("
        * C((1 - P")")^0)
        * P")"
        / function(desc, widthifexists_or_url, url)
            local txt = desc
            if next(desc) == nul then
              txt = ""
            end
            -- handle if no width is specified; then url is passed in 2nd argument
            url = url or widthifexists_or_url
            return pandoc.Image(txt, url)
          end ;
  -- MarkdownLink = P"["
  --              -- * C((1 - P"]")^1)
  --              * Ct(1^0)
  --              * P"]("
  --              * C(1^0)
  --              * P")"
  --              / pandoc.Plain ;
               -- * P"]("
               -- * C((1 - P"]")^0)
               -- * P")"
               -- / function(desc, url)
               --     -- local txt = desc or {pandoc.Str(url)}
               --     return pandoc.Link(desc, url)
               --   end ;
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
