self: super: {
  argon2 = self.callPackage ./argon2 {};
  asciidoc = self.callPackage ./asciidoc {};
  citeproc = self.callPackage ./citeproc {};
  commonmark = self.callPackage ./commonmark {};
  commonmark-extensions = self.callPackage ./commonmark-extensions {};
  commonmark-pandoc = self.callPackage ./commonmark-pandoc {};
  diagnostica = self.callPackage ./diagnostica {};
  diagnostica-sage = self.callPackage ./diagnostica-sage {};
  djot = self.callPackage ./djot {};
  doclayout = self.callPackage ./doclayout {};
  emojis = self.callPackage ./emojis {};
  http-api-data = self.callPackage ./http-api-data {};
  pandoc = self.callPackage ./pandoc {};
  pandoc-server = self.callPackage ./pandoc-server {};
  pandoc-types = self.callPackage ./pandoc-types {};
  sage = self.callPackage ./sage {};
  sage-parsers-instances = self.callPackage ./sage-parsers-instances {};
  temple = self.callPackage ./temple {};
  texmath = self.callPackage ./texmath {};
  tomlin = self.callPackage ./tomlin {};
  typst = self.callPackage ./typst {};
  typst-symbols = self.callPackage ./typst-symbols {};
}
