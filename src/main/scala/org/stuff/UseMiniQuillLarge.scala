package org.stuff

import org.stuff.MiniQuill._
import org.stuff.MiniQuill.Dsl._

object LargeModel {
  case class Foo(f1: String, f2: String, f3: String, f4: String, f5: String, f6: String, f7: String, f8: String, f9: String, f10: String, f11: String, f12: String, f13: String, f14: String, f15: String, f16: String, f17: String, f18: String, f19: String, f20: String, f21: String, f22: String, f23: String, f24: String, f25: String, f26: String, f27: String, f28: String, f29: String, f30: String, f31: String, f32: String, f33: String, f34: String, f35: String, f36: String, f37: String, f38: String, f39: String, f40: String, f41: String, f42: String, f43: String, f44: String, f45: String, f46: String, f47: String, f48: String, f49: String, f50: String, f51: String, f52: String, f53: String, f54: String, f55: String, f56: String, f57: String, f58: String, f59: String, f60: String, f61: String, f62: String, f63: String, f64: String, f65: String, f66: String, f67: String, f68: String, f69: String, f70: String, f71: String, f72: String, f73: String, f74: String, f75: String, f76: String, f77: String, f78: String, f79: String, f80: String, f81: String, f82: String, f83: String, f84: String, f85: String, f86: String, f87: String, f88: String, f89: String, f90: String, f91: String, f92: String, f93: String, f94: String, f95: String, f96: String, f97: String, f98: String, f99: String, f100: String)
  case class Bar(b1: String, b2: String, b3: String, b4: String, b5: String, b6: String, b7: String, b8: String, b9: String, b10: String, b11: String, b12: String, b13: String, b14: String, b15: String, b16: String, b17: String, b18: String, b19: String, b20: String, b21: String, b22: String, b23: String, b24: String, b25: String, b26: String, b27: String, b28: String, b29: String, b30: String, b31: String, b32: String, b33: String, b34: String, b35: String, b36: String, b37: String, b38: String, b39: String, b40: String, b41: String, b42: String, b43: String, b44: String, b45: String, b46: String, b47: String, b48: String, b49: String, b50: String, b51: String, b52: String, b53: String, b54: String, b55: String, b56: String, b57: String, b58: String, b59: String, b60: String, b61: String, b62: String, b63: String, b64: String, b65: String, b66: String, b67: String, b68: String, b69: String, b70: String, b71: String, b72: String, b73: String, b74: String, b75: String, b76: String, b77: String, b78: String, b79: String, b80: String, b81: String, b82: String, b83: String, b84: String, b85: String, b86: String, b87: String, b88: String, b89: String, b90: String, b91: String, b92: String, b93: String, b94: String, b95: String, b96: String, b97: String, b98: String, b99: String, b100: String)
  case class Baz(z1: String, z2: String, z3: String, z4: String, z5: String, z6: String, z7: String, z8: String, z9: String, z10: String, z11: String, z12: String, z13: String, z14: String, z15: String, z16: String, z17: String, z18: String, z19: String, z20: String, z21: String, z22: String, z23: String, z24: String, z25: String, z26: String, z27: String, z28: String, z29: String, z30: String, z31: String, z32: String, z33: String, z34: String, z35: String, z36: String, z37: String, z38: String, z39: String, z40: String, z41: String, z42: String, z43: String, z44: String, z45: String, z46: String, z47: String, z48: String, z49: String, z50: String, z51: String, z52: String, z53: String, z54: String, z55: String, z56: String, z57: String, z58: String, z59: String, z60: String, z61: String, z62: String, z63: String, z64: String, z65: String, z66: String, z67: String, z68: String, z69: String, z70: String, z71: String, z72: String, z73: String, z74: String, z75: String, z76: String, z77: String, z78: String, z79: String, z80: String, z81: String, z82: String, z83: String, z84: String, z85: String, z86: String, z87: String, z88: String, z89: String, z90: String, z91: String, z92: String, z93: String, z94: String, z95: String, z96: String, z97: String, z98: String, z99: String, z100: String)
  case class Blin(l1: String, l2: String, l3: String, l4: String, l5: String, l6: String, l7: String, l8: String, l9: String, l10: String, l11: String, l12: String, l13: String, l14: String, l15: String, l16: String, l17: String, l18: String, l19: String, l20: String, l21: String, l22: String, l23: String, l24: String, l25: String, l26: String, l27: String, l28: String, l29: String, l30: String, l31: String, l32: String, l33: String, l34: String, l35: String, l36: String, l37: String, l38: String, l39: String, l40: String, l41: String, l42: String, l43: String, l44: String, l45: String, l46: String, l47: String, l48: String, l49: String, l50: String, l51: String, l52: String, l53: String, l54: String, l55: String, l56: String, l57: String, l58: String, l59: String, l60: String, l61: String, l62: String, l63: String, l64: String, l65: String, l66: String, l67: String, l68: String, l69: String, l70: String, l71: String, l72: String, l73: String, l74: String, l75: String, l76: String, l77: String, l78: String, l79: String, l80: String, l81: String, l82: String, l83: String, l84: String, l85: String, l86: String, l87: String, l88: String, l89: String, l90: String, l91: String, l92: String, l93: String, l94: String, l95: String, l96: String, l97: String, l98: String, l99: String, l100: String)
  case class Wan(w1: String, w2: String, w3: String, w4: String, w5: String, w6: String, w7: String, w8: String, w9: String, w10: String, w11: String, w12: String, w13: String, w14: String, w15: String, w16: String, w17: String, w18: String, w19: String, w20: String, w21: String, w22: String, w23: String, w24: String, w25: String, w26: String, w27: String, w28: String, w29: String, w30: String, w31: String, w32: String, w33: String, w34: String, w35: String, w36: String, w37: String, w38: String, w39: String, w40: String, w41: String, w42: String, w43: String, w44: String, w45: String, w46: String, w47: String, w48: String, w49: String, w50: String, w51: String, w52: String, w53: String, w54: String, w55: String, w56: String, w57: String, w58: String, w59: String, w60: String, w61: String, w62: String, w63: String, w64: String, w65: String, w66: String, w67: String, w68: String, w69: String, w70: String, w71: String, w72: String, w73: String, w74: String, w75: String, w76: String, w77: String, w78: String, w79: String, w80: String, w81: String, w82: String, w83: String, w84: String, w85: String, w86: String, w87: String, w88: String, w89: String, w90: String, w91: String, w92: String, w93: String, w94: String, w95: String, w96: String, w97: String, w98: String, w99: String, w100: String)
  case class Kan(k1: String, k2: String, k3: String, k4: String, k5: String, k6: String, k7: String, k8: String, k9: String, k10: String, k11: String, k12: String, k13: String, k14: String, k15: String, k16: String, k17: String, k18: String, k19: String, k20: String, k21: String, k22: String, k23: String, k24: String, k25: String, k26: String, k27: String, k28: String, k29: String, k30: String, k31: String, k32: String, k33: String, k34: String, k35: String, k36: String, k37: String, k38: String, k39: String, k40: String, k41: String, k42: String, k43: String, k44: String, k45: String, k46: String, k47: String, k48: String, k49: String, k50: String, k51: String, k52: String, k53: String, k54: String, k55: String, k56: String, k57: String, k58: String, k59: String, k60: String, k61: String, k62: String, k63: String, k64: String, k65: String, k66: String, k67: String, k68: String, k69: String, k70: String, k71: String, k72: String, k73: String, k74: String, k75: String, k76: String, k77: String, k78: String, k79: String, k80: String, k81: String, k82: String, k83: String, k84: String, k85: String, k86: String, k87: String, k88: String, k89: String, k90: String, k91: String, k92: String, k93: String, k94: String, k95: String, k96: String, k97: String, k98: String, k99: String, k100: String)
}

object Queries {
  import LargeModel._

  inline def q1 = quote {
    for {
      f <- query[Foo]
      b <- query[Bar] if (f.f1 == b.b1 && f.f2 == b.b2 && f.f3 == b.b3 && f.f4 == b.b4 && f.f5 == b.b5 && f.f6 == b.b6 && f.f7 == b.b7 && f.f8 == b.b8 && f.f9 == b.b9 && f.f10 == b.b10 && f.f11 == b.b11 && f.f12 == b.b12 && f.f13 == b.b13 && f.f14 == b.b14 && f.f15 == b.b15 && f.f16 == b.b16 && f.f17 == b.b17 && f.f18 == b.b18 && f.f19 == b.b19 && f.f20 == b.b20 && f.f21 == b.b21 && f.f22 == b.b22 && f.f23 == b.b23 && f.f24 == b.b24 && f.f25 == b.b25 && f.f26 == b.b26 && f.f27 == b.b27 && f.f28 == b.b28 && f.f29 == b.b29 && f.f30 == b.b30 && f.f31 == b.b31 && f.f32 == b.b32 && f.f33 == b.b33 && f.f34 == b.b34 && f.f35 == b.b35 && f.f36 == b.b36 && f.f37 == b.b37 && f.f38 == b.b38 && f.f39 == b.b39 && f.f40 == b.b40 && f.f41 == b.b41 && f.f42 == b.b42 && f.f43 == b.b43 && f.f44 == b.b44 && f.f45 == b.b45 && f.f46 == b.b46 && f.f47 == b.b47 && f.f48 == b.b48 && f.f49 == b.b49 && f.f50 == b.b50 && f.f51 == b.b51 && f.f52 == b.b52 && f.f53 == b.b53 && f.f54 == b.b54 && f.f55 == b.b55 && f.f56 == b.b56 && f.f57 == b.b57 && f.f58 == b.b58 && f.f59 == b.b59 && f.f60 == b.b60 && f.f61 == b.b61 && f.f62 == b.b62 && f.f63 == b.b63 && f.f64 == b.b64 && f.f65 == b.b65 && f.f66 == b.b66 && f.f67 == b.b67 && f.f68 == b.b68 && f.f69 == b.b69 && f.f70 == b.b70 && f.f71 == b.b71 && f.f72 == b.b72 && f.f73 == b.b73 && f.f74 == b.b74 && f.f75 == b.b75 && f.f76 == b.b76 && f.f77 == b.b77 && f.f78 == b.b78 && f.f79 == b.b79 && f.f80 == b.b80 && f.f81 == b.b81 && f.f82 == b.b82 && f.f83 == b.b83 && f.f84 == b.b84 && f.f85 == b.b85 && f.f86 == b.b86 && f.f87 == b.b87 && f.f88 == b.b88 && f.f89 == b.b89 && f.f90 == b.b90 && f.f91 == b.b91 && f.f92 == b.b92 && f.f93 == b.b93 && f.f94 == b.b94 && f.f95 == b.b95 && f.f96 == b.b96 && f.f97 == b.b97 && f.f98 == b.b98 && f.f99 == b.b99 && f.f100 == b.b100)
    } yield b
  }

  inline def q2 = quote {
    for {
      b <- q1
      z <- query[Baz] if (b.b1 == z.z1 && b.b2 == z.z2 && b.b3 == z.z3 && b.b4 == z.z4 && b.b5 == z.z5 && b.b6 == z.z6 && b.b7 == z.z7 && b.b8 == z.z8 && b.b9 == z.z9 && b.b10 == z.z10 && b.b11 == z.z11 && b.b12 == z.z12 && b.b13 == z.z13 && b.b14 == z.z14 && b.b15 == z.z15 && b.b16 == z.z16 && b.b17 == z.z17 && b.b18 == z.z18 && b.b19 == z.z19 && b.b20 == z.z20 && b.b21 == z.z21 && b.b22 == z.z22 && b.b23 == z.z23 && b.b24 == z.z24 && b.b25 == z.z25 && b.b26 == z.z26 && b.b27 == z.z27 && b.b28 == z.z28 && b.b29 == z.z29 && b.b30 == z.z30 && b.b31 == z.z31 && b.b32 == z.z32 && b.b33 == z.z33 && b.b34 == z.z34 && b.b35 == z.z35 && b.b36 == z.z36 && b.b37 == z.z37 && b.b38 == z.z38 && b.b39 == z.z39 && b.b40 == z.z40 && b.b41 == z.z41 && b.b42 == z.z42 && b.b43 == z.z43 && b.b44 == z.z44 && b.b45 == z.z45 && b.b46 == z.z46 && b.b47 == z.z47 && b.b48 == z.z48 && b.b49 == z.z49 && b.b50 == z.z50 && b.b51 == z.z51 && b.b52 == z.z52 && b.b53 == z.z53 && b.b54 == z.z54 && b.b55 == z.z55 && b.b56 == z.z56 && b.b57 == z.z57 && b.b58 == z.z58 && b.b59 == z.z59 && b.b60 == z.z60 && b.b61 == z.z61 && b.b62 == z.z62 && b.b63 == z.z63 && b.b64 == z.z64 && b.b65 == z.z65 && b.b66 == z.z66 && b.b67 == z.z67 && b.b68 == z.z68 && b.b69 == z.z69 && b.b70 == z.z70 && b.b71 == z.z71 && b.b72 == z.z72 && b.b73 == z.z73 && b.b74 == z.z74 && b.b75 == z.z75 && b.b76 == z.z76 && b.b77 == z.z77 && b.b78 == z.z78 && b.b79 == z.z79 && b.b80 == z.z80 && b.b81 == z.z81 && b.b82 == z.z82 && b.b83 == z.z83 && b.b84 == z.z84 && b.b85 == z.z85 && b.b86 == z.z86 && b.b87 == z.z87 && b.b88 == z.z88 && b.b89 == z.z89 && b.b90 == z.z90 && b.b91 == z.z91 && b.b92 == z.z92 && b.b93 == z.z93 && b.b94 == z.z94 && b.b95 == z.z95 && b.b96 == z.z96 && b.b97 == z.z97 && b.b98 == z.z98 && b.b99 == z.z99 && b.b100 == z.z100)
      l <- query[Blin] if (z.z1 == l.l1 && z.z2 == l.l2 && z.z3 == l.l3 && z.z4 == l.l4 && z.z5 == l.l5 && z.z6 == l.l6 && z.z7 == l.l7 && z.z8 == l.l8 && z.z9 == l.l9 && z.z10 == l.l10 && z.z11 == l.l11 && z.z12 == l.l12 && z.z13 == l.l13 && z.z14 == l.l14 && z.z15 == l.l15 && z.z16 == l.l16 && z.z17 == l.l17 && z.z18 == l.l18 && z.z19 == l.l19 && z.z20 == l.l20 && z.z21 == l.l21 && z.z22 == l.l22 && z.z23 == l.l23 && z.z24 == l.l24 && z.z25 == l.l25 && z.z26 == l.l26 && z.z27 == l.l27 && z.z28 == l.l28 && z.z29 == l.l29 && z.z30 == l.l30 && z.z31 == l.l31 && z.z32 == l.l32 && z.z33 == l.l33 && z.z34 == l.l34 && z.z35 == l.l35 && z.z36 == l.l36 && z.z37 == l.l37 && z.z38 == l.l38 && z.z39 == l.l39 && z.z40 == l.l40 && z.z41 == l.l41 && z.z42 == l.l42 && z.z43 == l.l43 && z.z44 == l.l44 && z.z45 == l.l45 && z.z46 == l.l46 && z.z47 == l.l47 && z.z48 == l.l48 && z.z49 == l.l49 && z.z50 == l.l50 && z.z51 == l.l51 && z.z52 == l.l52 && z.z53 == l.l53 && z.z54 == l.l54 && z.z55 == l.l55 && z.z56 == l.l56 && z.z57 == l.l57 && z.z58 == l.l58 && z.z59 == l.l59 && z.z60 == l.l60 && z.z61 == l.l61 && z.z62 == l.l62 && z.z63 == l.l63 && z.z64 == l.l64 && z.z65 == l.l65 && z.z66 == l.l66 && z.z67 == l.l67 && z.z68 == l.l68 && z.z69 == l.l69 && z.z70 == l.l70 && z.z71 == l.l71 && z.z72 == l.l72 && z.z73 == l.l73 && z.z74 == l.l74 && z.z75 == l.l75 && z.z76 == l.l76 && z.z77 == l.l77 && z.z78 == l.l78 && z.z79 == l.l79 && z.z80 == l.l80 && z.z81 == l.l81 && z.z82 == l.l82 && z.z83 == l.l83 && z.z84 == l.l84 && z.z85 == l.l85 && z.z86 == l.l86 && z.z87 == l.l87 && z.z88 == l.l88 && z.z89 == l.l89 && z.z90 == l.l90 && z.z91 == l.l91 && z.z92 == l.l92 && z.z93 == l.l93 && z.z94 == l.l94 && z.z95 == l.l95 && z.z96 == l.l96 && z.z97 == l.l97 && z.z98 == l.l98 && z.z99 == l.l99 && z.z100 == l.l100)
    } yield l
  }

  inline def q3 = quote {
    for {
      l <- q2
      w <- query[Wan] if (l.l1 == w.w1 && l.l2 == w.w2 && l.l3 == w.w3 && l.l4 == w.w4 && l.l5 == w.w5 && l.l6 == w.w6 && l.l7 == w.w7 && l.l8 == w.w8 && l.l9 == w.w9 && l.l10 == w.w10 && l.l11 == w.w11 && l.l12 == w.w12 && l.l13 == w.w13 && l.l14 == w.w14 && l.l15 == w.w15 && l.l16 == w.w16 && l.l17 == w.w17 && l.l18 == w.w18 && l.l19 == w.w19 && l.l20 == w.w20 && l.l21 == w.w21 && l.l22 == w.w22 && l.l23 == w.w23 && l.l24 == w.w24 && l.l25 == w.w25 && l.l26 == w.w26 && l.l27 == w.w27 && l.l28 == w.w28 && l.l29 == w.w29 && l.l30 == w.w30 && l.l31 == w.w31 && l.l32 == w.w32 && l.l33 == w.w33 && l.l34 == w.w34 && l.l35 == w.w35 && l.l36 == w.w36 && l.l37 == w.w37 && l.l38 == w.w38 && l.l39 == w.w39 && l.l40 == w.w40 && l.l41 == w.w41 && l.l42 == w.w42 && l.l43 == w.w43 && l.l44 == w.w44 && l.l45 == w.w45 && l.l46 == w.w46 && l.l47 == w.w47 && l.l48 == w.w48 && l.l49 == w.w49 && l.l50 == w.w50 && l.l51 == w.w51 && l.l52 == w.w52 && l.l53 == w.w53 && l.l54 == w.w54 && l.l55 == w.w55 && l.l56 == w.w56 && l.l57 == w.w57 && l.l58 == w.w58 && l.l59 == w.w59 && l.l60 == w.w60 && l.l61 == w.w61 && l.l62 == w.w62 && l.l63 == w.w63 && l.l64 == w.w64 && l.l65 == w.w65 && l.l66 == w.w66 && l.l67 == w.w67 && l.l68 == w.w68 && l.l69 == w.w69 && l.l70 == w.w70 && l.l71 == w.w71 && l.l72 == w.w72 && l.l73 == w.w73 && l.l74 == w.w74 && l.l75 == w.w75 && l.l76 == w.w76 && l.l77 == w.w77 && l.l78 == w.w78 && l.l79 == w.w79 && l.l80 == w.w80 && l.l81 == w.w81 && l.l82 == w.w82 && l.l83 == w.w83 && l.l84 == w.w84 && l.l85 == w.w85 && l.l86 == w.w86 && l.l87 == w.w87 && l.l88 == w.w88 && l.l89 == w.w89 && l.l90 == w.w90 && l.l91 == w.w91 && l.l92 == w.w92 && l.l93 == w.w93 && l.l94 == w.w94 && l.l95 == w.w95 && l.l96 == w.w96 && l.l97 == w.w97 && l.l98 == w.w98 && l.l99 == w.w99 && l.l100 == w.w100)
      k <- query[Kan] if (w.w1 == k.k1 && w.w2 == k.k2 && w.w3 == k.k3 && w.w4 == k.k4 && w.w5 == k.k5 && w.w6 == k.k6 && w.w7 == k.k7 && w.w8 == k.k8 && w.w9 == k.k9 && w.w10 == k.k10 && w.w11 == k.k11 && w.w12 == k.k12 && w.w13 == k.k13 && w.w14 == k.k14 && w.w15 == k.k15 && w.w16 == k.k16 && w.w17 == k.k17 && w.w18 == k.k18 && w.w19 == k.k19 && w.w20 == k.k20 && w.w21 == k.k21 && w.w22 == k.k22 && w.w23 == k.k23 && w.w24 == k.k24 && w.w25 == k.k25 && w.w26 == k.k26 && w.w27 == k.k27 && w.w28 == k.k28 && w.w29 == k.k29 && w.w30 == k.k30 && w.w31 == k.k31 && w.w32 == k.k32 && w.w33 == k.k33 && w.w34 == k.k34 && w.w35 == k.k35 && w.w36 == k.k36 && w.w37 == k.k37 && w.w38 == k.k38 && w.w39 == k.k39 && w.w40 == k.k40 && w.w41 == k.k41 && w.w42 == k.k42 && w.w43 == k.k43 && w.w44 == k.k44 && w.w45 == k.k45 && w.w46 == k.k46 && w.w47 == k.k47 && w.w48 == k.k48 && w.w49 == k.k49 && w.w50 == k.k50 && w.w51 == k.k51 && w.w52 == k.k52 && w.w53 == k.k53 && w.w54 == k.k54 && w.w55 == k.k55 && w.w56 == k.k56 && w.w57 == k.k57 && w.w58 == k.k58 && w.w59 == k.k59 && w.w60 == k.k60 && w.w61 == k.k61 && w.w62 == k.k62 && w.w63 == k.k63 && w.w64 == k.k64 && w.w65 == k.k65 && w.w66 == k.k66 && w.w67 == k.k67 && w.w68 == k.k68 && w.w69 == k.k69 && w.w70 == k.k70 && w.w71 == k.k71 && w.w72 == k.k72 && w.w73 == k.k73 && w.w74 == k.k74 && w.w75 == k.k75 && w.w76 == k.k76 && w.w77 == k.k77 && w.w78 == k.k78 && w.w79 == k.k79 && w.w80 == k.k80 && w.w81 == k.k81 && w.w82 == k.k82 && w.w83 == k.k83 && w.w84 == k.k84 && w.w85 == k.k85 && w.w86 == k.k86 && w.w87 == k.k87 && w.w88 == k.k88 && w.w89 == k.k89 && w.w90 == k.k90 && w.w91 == k.k91 && w.w92 == k.k92 && w.w93 == k.k93 && w.w94 == k.k94 && w.w95 == k.k95 && w.w96 == k.k96 && w.w97 == k.k97 && w.w98 == k.k98 && w.w99 == k.k99 && w.w100 == k.k100)
    } yield k
  }
}

object UseMiniQuillLarge {
  import Queries._

  def main(args: Array[String]):Unit = {
    println(q3)
  }
}