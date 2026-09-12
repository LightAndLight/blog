self: super: {
  argon2 = self.callPackage ./argon2 {};
  diagnostica = self.callPackage ./diagnostica {};
  diagnostica-sage = self.callPackage ./diagnostica-sage {};
  sage = self.callPackage ./sage {};
  sage-parsers-instances = self.callPackage ./sage-parsers-instances {};
  temple = self.callPackage ./temple {};
  tomlin = self.callPackage ./tomlin {};
}
