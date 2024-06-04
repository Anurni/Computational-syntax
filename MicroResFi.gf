resource MicroResFi = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Gen | Par | Ine | Ela | Ill | Ade | Abl | All | Ess | Tra | Abe ; --left out two cases: instruktiivi and komitatiivi
  Tense = Pres | Per | Pkp ;  --present (fi:preesens), present perfect (fi:perfekti) and past perfect (fi:pluskvamperfekti)
  Person = Per1 | Per2 | Per3 ;

  Agreement = NAgr Number Case ; -- nouns and adjectives agree with nouns in number and case
  AgreementVerbs = VAgr Number Person Tense ; --Tense not needed maybe?

  Verbform = IndPres | IndPer | IndPkp ; --not used


oper

-- OPERS TO HELP WITH VOWEL HARMONY --

  vowelHarmony : Str -> Str ;     --trying to minimize pattern matching repetition with this oper that is used for the vowel harmony checking
  vowelHarmony = \c -> case c of {
    "a" => "a";
    "o" => "a";
    "u" => "a";
    "e" => "ä";
    "i" => "ä";
    "ä" => "ä";
    "y" => "ä";
    "ö" => "ä"
} ;
  otherVowelHarmony : Str -> Str ;   --another vowel harmony oper to help us
  otherVowelHarmony = \character -> case character of {
    x + ("a"|"o"|"u") + y => "a";
    _ => "ä"
  };

  illativeVowelHarmony : Str -> Str ;  --needed for the illative case
  illativeVowelHarmony = \character -> case character of {
    x + "o" => "oo";
    x + "u" => "uu";
    x + "a" => "aa";
    x + ("i"|"e") => "ee";
    x + "ä" => "ää";
    x + "ö" => "öö"
  };

  pastParticipleVowelHarmony : Str -> Str ; --needed for past participle vowel harmony
  pastParticipleVowelHarmony = \character -> case character of {
    "ä" => "y";
    "a" => "u"
  };

  verbHelpVowelHarmony : Str -> Str ;
  verbHelpVowelHarmony = \verb -> case verb of {
    x + "o" => "o";
    x + "a" => "a";
    x + "ä" => "ä";
    x + "u" => "u"
  };

  ekstraKonsonantti : Str -> Str ;
  ekstraKonsonantti = \verb -> case last verb of {
    "l" => "l";
    "t" => "t";
    "k" => "k";
    "p" => "p";
    "n" => "n"
  };

-- this oper will be used with 3rd person sg when conjugating  verbs
  doubleVowel : Str -> Str ;
  doubleVowel = \stem -> case stem of {
      x + "a" =>  "aa";
      x + "e" =>  "ee";
      x + "u" =>  "uu";
      x + "o" =>  "oo";
      x + "y" =>  "yy";
      x + "i" =>  "ii";
      x + "ä" =>  "ää";
      x + "ö" =>  "öö"
  };

--  NOUNS --

-- declaring the noun type in Finnish
  Noun : Type = {s : Number => Case => Str} ;

-- NOUN - MAKING OPER !
  mkNoun : Str -> Str -> Noun = \sg,pl -> {  
    s = table {
        Sg => table {
            Nom => sg ; 
            Acc => sg + "n" ; 
            Gen => sg + "n" ; 
            Par => case sg of { 
              x + ("ää"|"aa"|"uu"|"s"|"r"|"n"|"t") => sg + "t" + vowelHarmony(last sg) ;
              x + "e" => sg + "tt" + otherVowelHarmony(sg) ;  
              x + ("a"|"o"|"u"|"y"|"i"|"ä"|"ö") => sg + vowelHarmony(last sg)  
            } ;
            Ine => sg + "ss" + vowelHarmony(last sg);
            Ela => sg + "st" + vowelHarmony(last sg);
            Ill => case sg of { --taloon vs. laivaan vs. halpahalliin according to the 
            x + "uu" => sg + "hun";
            x + "su" => sg + "un";
            x + ("a"|"o"|"u"|"y"|"i"|"ä"|"ö") => x + illativeVowelHarmony(sg) + "n";
            x + "e" => sg + "eseen";
            x + "s" => x + "heen" 
            };
            Ade => sg + "ll" + vowelHarmony(last sg); 
            Abl => sg + "lt" + vowelHarmony(last sg); 
            All => sg + "lle" ; --laivalle, talolle, leivälle...
            Ess => sg + "n" + vowelHarmony(last sg);
            Tra => sg + "ksi" ; --laivaksi
            Abe => sg + "tt" + vowelHarmony(last sg)
        };
        Pl => table {
            Nom => pl ; 
            Acc => pl ; 
            Gen => case sg of {  
            x + "eä" => x + "eiden";  
            x + ("ka"| "i"|"ä"|"na"|"ra") => init sg + "ien" ; --poika poikien, tuli tulien viini viinien lehmä lehmien...
            x + ("o"|"a") => x + "ojen" ;
            x + "su" => sg + "jen" ;
            x + "uu" => x + "uiden";
            ("r"| "n" | "t"| "s") => "ten"  --miesten, tytärten 
            } ;
            Par => case sg of {  
            x + "eä" => x + "eitä"; 
            x + "su" => x + "ja";
            x + ("ka"|"na"|"ra") => init sg + "ia" ; --poika poikia, tuli tulia
            x + ("o"|"a") => init sg + "oja" ;
            x + "uu" => x + "uita";
            x + "ä" => init sg + "iä" ;
            x + "i" => init sg + "ejä"
            } ;
            Ine => case sg of {
            x + ("to"|"su") => sg + "iss" + vowelHarmony(last sg);
            x + ("i"|"ä"|"na"|"uu") => init sg + "iss" + vowelHarmony(last sg) ;
            _ => init sg + "oiss" + vowelHarmony(last sg)
            };
            Ela => case sg of {
            x + ("to"|"su") => sg + "ist" + vowelHarmony(last sg);
            x + ("i"|"ä"|"na"|"uu") => init sg + "ist" + vowelHarmony(last sg) ;
            _ => init sg + "oist" + vowelHarmony(last sg)
            };
            Ill => case sg of {  --vowel harmony needs to be taken into account
            x + "eä" => x + "eisiin"; 
            x + "su" => sg + "ihin";
            x + "to" => x + "ihin" ; --back vowels
            x + ("a" | "sa"| "pi"| "va") => init sg + "oihin" ; --front vowels
            _ => init sg + "iin"
            };
            Ade => case sg of {
              x + ("mä"| "na"|"vä"|"rä"|"nen") => init sg + "ill" + vowelHarmony(last sg);
              x + ("va"|"ra") => init sg + "oill" + vowelHarmony(last sg);
              x + "a" => init sg + "oill" + vowelHarmony(last sg);
              _ => sg + "ill" + vowelHarmony(last sg)
            };
            Abl => case sg of {
              x + ("mä"|"na"|"vä"|"sa"|"rä"|"nen") => init sg + "ilt" + vowelHarmony(last sg);
              x + ("va"|"ra") => init sg + "oilt" + vowelHarmony(last sg);
              x + "a" => init sg + "oilt" + vowelHarmony(last sg);
              _ => sg + "ilt" + vowelHarmony(last sg)
            };
            All => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oille";
              x + ("rä"|"vä"|"mä") => init sg + "ille";
              x + "a" => init sg + "oill" + vowelHarmony(last sg);
              _ => sg + "ille"
            };
            Ess => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oin" + vowelHarmony(last sg);
              x + ("mä"|"rä"|"vä"|"nä") => init sg + "in" + vowelHarmony(last sg);
              x + "a" => init sg + "oin" + vowelHarmony(last sg);
              _ => sg + "in" + vowelHarmony(last sg)
            };
            Tra => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oiksi";
              x + ("mä"|"rä"|"vä"|"nä") => init sg + "iksi";
              x + "a" => init sg + "oiksi";
              _ => sg + "iksi"
            };
            Abe => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oitt" + vowelHarmony(last sg);
              x + ("mä"|"rä"|"vä"|"nä") => init sg + "itt" + vowelHarmony(last sg);
              x + "a" => init sg + "oitt" + vowelHarmony(last sg);
              _ => sg + "itt" + vowelHarmony(last sg)
            }
        }
    }
} ;

  -- regular paradigm for regular cases like talo talot, vauva vauvat, juna junat, laiva laivat, viini viinit, koira koirat....
  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "t") ;

  -- smart paradigms (trying to implement at least the lemma-modifying KPT (strong-weak variation) rule ) to cover words like tyttö -> tytöt, kukka -> kukat, kielioppi -> kieliopit...
  -- also some other nouns that are a bit "less regular", lapsi  - lapset, virhe - virheet, punainen - punaisen, olut - oluet, mies - miehet
  -- trying to implement noun case inflection for kpt nouns, aiming to cover similar words that are not even necessarily in the lexicon
  -- also added pattern matching for adjectives bc their inflections will be reached from this table
smartNoun : Str -> Noun = \sg -> {
  s = table {
    Sg => table {
      Nom => sg ;
      Acc => case sg of {
        x + "ttö" => x + "tön" ; --tt -> i
        x + "akku" => x + "akun" ; --kk -> k
        x + "attu" => x + "atun" ; --tt -> t
        x + "appu" => x + "apun" ; --pp -> p
        x + "akko" => x + "akon" ; --kk -> k
        x + "atto" => x + "aton" ; --tt -> t
        x + "akki" => x + "akin" ; --kk -> k
        x + "appi" => x + "apin" ; --pp -> p
        x + "akka" => x + "akan" ; --kk -> k
        x + "kki" => x + "kin" ; --kk -> k
        x + "kka" => x + "kan" ; --kk -> k
        x + "ppi" => x + "pin" ; --pp -> p
        x + "tti" => x + "tin" ; --tt -> i
        x + "ttö" => x + "tön" ; --tt -> i
        x + "es" => x + "ehen";
        x + "tu" => x + "tua" ; 
        x + ("i"|"t") => x + "en" ; --lapsi, olut
        x + "s" => x + "ksen" ; 
        x + "e" => x + "een" ; -- virhe, virheet
        x + "nen" => x + "sen";
        x + "in" => init sg + "men";
        _ => (regNoun sg).s ! Sg ! Acc 
      } ;
      Gen => case sg of {
        x + "akku" => x + "akun" ; --kk -> k
        x + "attu" => x + "atun" ; --tt -> t
        x + "appu" => x + "apun" ; --pp -> p
        x + "akko" => x + "akon" ; --kk -> k
        x + "atto" => x + "aton" ; --tt -> t
        x + "akki" => x + "akin" ; --kk -> k
        x + "appi" => x + "apin" ; --pp -> p
        x + "akka" => x + "akan" ; --kk -> k
        x + "kki" => x + "kin" ; --kk -> k
        x + "kka" => x + "kan" ; --kk -> k
        x + "ppi" => x + "pin" ; --pp -> p
        x + "tti" => x + "tin" ; --tt -> i
        x + "ttö" => x + "tön" ; --tt -> i
        x + "es" => x + "ehen";
        x + "tu" => x + "nun" ; 
        x + ("i"|"t") => x + "en" ; --lapsi, olut
        x + "s" => x + "ksen" ; 
        x + "e" => x + "een" ; -- tietokoneen, veneen, 
        x + "nen" => x + "sen";
        x + "in" => init sg + "men";
        _ => (regNoun sg).s ! Sg ! Gen 
      };
      Par => case sg of {
        x + "nen" => x + "st" + otherVowelHarmony(sg);
        x + "ppi" => sg + "a";
        x + "es" => x + "es" + otherVowelHarmony(sg);
        x + "tu" => x + "tua";
        x + "n" => x + "tä";
        x + "e" => x + "että";
        x + "t" => x + "t" + otherVowelHarmony(sg);
        x + ("i"|"s") => x + "t" + vowelHarmony(last sg);
        x + "a" => x + "aa";
        x + "u" => x + "ua";
        x + "o" => x + "oa";
        x + "ö" => x + "öä";
        x + "ä" => x + "ää" ;
        _ => (regNoun sg).s ! Sg ! Par 
      } ;
      Ine => case sg of {
        x + "akku" => x + "akussa" ; --kk -> k
        x + "attu" => x + "atussa" ; --tt -> t
        x + "appu" => x + "apussa" ; --pp -> p
        x + "akko" => x + "akossa" ; --kk -> k
        x + "atto" => x + "atossa" ; --tt -> t
        x + "akki" => x + "akissa" ; --kk -> k
        x + "appi" => x + "apissa" ; --pp -> p
        x + "akka" => x + "akassa" ; --kk -> k
        x + "kki" => x + "kissa" ; --kk -> k
        x + "kka" => x + "kassa" ; --kk -> k
        x + "ppi" => x + "pissa" ; --pp -> p
        x + "tti" => x + "tissa" ; --tt -> i
        x + "ttö" => x + "tössä" ; --tt -> i
        x + "es" => x + "ehess" + otherVowelHarmony(sg);
        x + "tu" => x + "nussa" ; 
        x + "i" => x + "ess" + vowelHarmony(last sg) ; --lapsi
        x + "t" => x + "ess" + otherVowelHarmony(sg);
        x + "s" => x + "ksessa" ; 
        x + "e" => x + "eessä" ;  -- virhe, virheet
        x + "nen" => x + "sess" + otherVowelHarmony(sg);
        x + "in" => init sg + "mess" + otherVowelHarmony(sg);
        _ => (regNoun sg).s ! Sg ! Ine 
      } ;
      Ela => case sg of {
        x + "akku" => x + "akusta" ; --kk -> k
        x + "attu" => x + "atusta" ; --tt -> t
        x + "appu" => x + "apusta" ; --pp -> p
        x + "akko" => x + "akosta" ; --kk -> k
        x + "atto" => x + "atosta" ; --tt -> t
        x + "akki" => x + "akista" ; --kk -> k
        x + "appi" => x + "apista" ; --pp -> p
        x + "akka" => x + "akasta" ; --kk -> k
        x + "kki" => x + "kista" ; --kk -> k
        x + "kka" => x + "kasta" ; --kk -> k
        x + "ppi" => x + "pista" ; --pp -> p
        x + "tti" => x + "tista" ; --tt -> i
        x + "ttö" => x + "töstä" ; --tt -> i
        x + "tu" => x + "nusta" ; 
        x + "es" => x + "est" + otherVowelHarmony(sg);
        x + "t" => x + "ess" + otherVowelHarmony(sg);
        x + "i" => x + "est" + vowelHarmony(last sg) ; --lapsi
        x + "s" => x + "ksesta" ; 
        x + "e" => x + "eestä" ; -- virhe, virheet
        x + "nen" => x + "sest" + otherVowelHarmony(sg);
        x + "in" => init sg + "mest" + otherVowelHarmony(sg);
        _ => (regNoun sg).s ! Sg ! Ela 
      } ;
      Ill => case sg of {
        x + ("li"|"ri"|"si"|"t") => init sg + "een";
        x + "es" => x + "heen";
        x + "ö" => x + "öön";
        x + "nen" => x + "seen";
        x + "a" => x + "aan" ;
        x + "u" => x + "un" ;
        x + "o" => x + "on" ;
        x + "s" => x + "en" ;
        x + "e" => x + "seen";
        x + "i" => x + "iin";
        x + "in" => init sg + "meen";
        _ => (regNoun sg).s ! Sg ! Ill 
      } ;
      Ade => case sg of {
        x + "akku" => x + "akulla" ; --kk -> k
        x + "attu" => x + "atulla" ; --tt -> t
        x + "appu" => x + "apulla" ; --pp -> p
        x + "akko" => x + "akolla" ; --kk -> k
        x + "atto" => x + "atolla" ; --tt -> t
        x + "akki" => x + "akilla" ; --kk -> k
        x + "appi" => x + "apilla" ; --pp -> p
        x + "akka" => x + "akalla" ; --kk -> k
        x + "kki" => x + "killa" ; --kk -> k
        x + "kka" => x + "kalla" ; --kk -> k
        x + "ppi" => x + "pilla" ; --pp -> p
        x + "tti" => x + "tilla" ; --tt -> i
        x + "ttö" => x + "töllä" ; --tt -> i
        x + "tu" => x + "nulla" ; 
        x + "es" => x + "ehell" + otherVowelHarmony(sg);
        x + "t" => x + "ell" + otherVowelHarmony(sg);
        x + "i" => x + "ell" + vowelHarmony(last sg) ; 
        x + "s" => x + "ksella" ; 
        x + "e" => x + "eellä" ; -- virhe, virheet
        x + "nen" => x + "sell" + otherVowelHarmony(sg);
        x + "in" => init sg + "mell" + otherVowelHarmony(sg);
        _ => (regNoun sg).s ! Sg ! Ade 
      } ;
      Abl => case sg of {
        x + "akku" => x + "akulta" ; --kk -> k
        x + "attu" => x + "atulta" ; --tt -> t
        x + "appu" => x + "apulta" ; --pp -> p
        x + "akko" => x + "akolta" ; --kk -> k
        x + "atto" => x + "atolta" ; --tt -> t
        x + "akki" => x + "akilta" ; --kk -> k
        x + "appi" => x + "apilta" ; --pp -> p
        x + "akka" => x + "akalta" ; --kk -> k
        x + "kki" => x + "kilta" ; --kk -> k
        x + "kka" => x + "kalta" ; --kk -> k
        x + "ppi" => x + "pilta" ; --pp -> p
        x + "tti" => x + "tilta" ; --tt -> i
        x + "ttö" => x + "töltä" ; --tt -> i
        x + "tu" => x + "nulta" ; 
        x + "es" => x + "ehelt" + otherVowelHarmony(sg);
        x + "t" => x + "elt" + otherVowelHarmony(sg);
        x + "i" => x + "elt" + vowelHarmony(last sg) ; 
        x + "s" => x + "kselta" ; 
        x + "e" => x + "eeltä" ; -- virhe, virheet
        x + "nen" => x + "selt" + otherVowelHarmony(sg);
        x + "in" => init sg + "melt" + otherVowelHarmony(sg);
        _ => (regNoun sg).s ! Sg ! Abl 
      } ;
      All => case sg of {
        x + "akku" => x + "akulle" ; --kk -> k
        x + "attu" => x + "atulle" ; --tt -> t
        x + "appu" => x + "apulle" ; --pp -> p
        x + "akko" => x + "akolle" ; --kk -> k
        x + "atto" => x + "atolle" ; --tt -> t
        x + "akki" => x + "akille" ; --kk -> k
        x + "appi" => x + "apille" ; --pp -> p
        x + "akka" => x + "akalle" ; --kk -> k
        x + "kki" => x + "kille" ; --kk -> k
        x + "kka" => x + "kalle" ; --kk -> k
        x + "ppi" => x + "pille" ; --pp -> p
        x + "tti" => x + "tille" ; --tt -> i
        x + "ttö" => x + "tölle" ; --tt -> i
        x + "es" => x + "ehelle";
        x + "tu" => x + "nulle" ; 
        x + ("i"|"t") => x + "elle" ; --lapsi olut
        x + "s" => x + "kselle" ; 
        x + "e" => x + "eelle" ; -- virhe, virheet
        x + "nen" => x + "selle";
        x + "in" => init sg + "melle";
        _ => (regNoun sg).s ! Sg ! All 
      } ;
      Ess => case sg of {
        x + "ki" => x + "kina";
        x + "nen" => x + "sen" + otherVowelHarmony(sg) ;
        x + ("pi"|"a"|"o"|"u"|"t") => sg + "na" ;
        x + "es" => x + "ehen" + otherVowelHarmony(sg);
        x + ("i"|"e") => x + "enä" ; 
        x + ("ö"|"ä") => sg + "nä";
        x + "in" => init sg + "menä";
        _ => (regNoun sg).s ! Sg ! Ess
      } ;
      Tra => case sg of {
        x + "akku" => x + "akuksi" ; --kk -> k
        x + "attu" => x + "atuksi" ; --tt -> t
        x + "appu" => x + "apuksi" ; --pp -> p
        x + "akko" => x + "akoksi" ; --kk -> k
        x + "atto" => x + "atoksi" ; --tt -> t
        x + "akki" => x + "akiksi" ; --kk -> k
        x + "appi" => x + "apiksi" ; --pp -> p
        x + "akka" => x + "akaksi" ; --kk -> k
        x + "kki" => x + "kiksi" ; --kk -> k
        x + "kka" => x + "kaksi" ; --kk -> k
        x + "ppi" => x + "piksi" ; --pp -> p
        x + "tti" => x + "tiksi" ; --tt -> i
        x + "ttö" => x + "töksi" ; --tt -> i
        x + "es" => x + "eheksi";
        x + "tu" => x + "nuksi" ; 
        x + ("i"|"t") => x + "eksi" ; --lapsi, olut
        x + "s" => x + "kseksi" ; 
        x + "e" => x + "eeksi" ; -- virhe, virheet
        x + "nen" => x + "seksi";
        x + "in" => init sg + "meksi";
        _ => (regNoun sg).s ! Sg ! Tra  
      } ;
      Abe => case sg of {
        x + "akku" => x + "akutta" ; --kk -> k
        x + "attu" => x + "atutta" ; --tt -> t
        x + "appu" => x + "aputta" ; --pp -> p
        x + "akko" => x + "akotta" ; --kk -> k
        x + "atto" => x + "atotta" ; --tt -> t
        x + "akki" => x + "akitta" ; --kk -> k
        x + "appi" => x + "apitta" ; --pp -> p
        x + "akka" => x + "akatta" ; --kk -> k
        x + "kki" => x + "katta" ; --kk -> k
        x + "kka" => x + "katta" ; --kk -> k
        x + "ppi" => x + "pitta" ; --pp -> p
        x + "tti" => x + "titta" ; --tt -> i
        x + "ttö" => x + "töttä" ; --tt -> i
        x + "es" => x + "ehett" + otherVowelHarmony(sg);
        x + "tu" => x + "nutta" ; 
        x + "t" => x + "ett" + otherVowelHarmony(sg);
        x + "i" => x + "ett" + vowelHarmony(last sg) ; 
        x + "s" => x + "ksetta" ; 
        x + "e" => x + "eettä" ; -- virhe, virheet
        x + "nen" => x + "sett" + otherVowelHarmony(sg);
        x + "in" => init sg + "mettä";
        _ => (regNoun sg).s ! Sg ! Abe 
      } 
    };
    Pl => table {
      Nom => case sg of {
      x + "akku" => x + "akut" ;
      x + "attu" => x + "atut" ; 
      x + "appu" => x + "aput" ; 
      x + "akko" => x + "akot" ; 
      x + "atto" => x + "atot" ; 
      x + "akki" => x + "akit" ; 
      x + "appi" => x + "apit" ; 
      x + "akka" => x + "akat" ; 
      x + "kki" => x + "kit" ; 
      x + "kka" => x + "kat" ; 
      x + "ppi" => x + "pit" ; 
      x + "tti" => x + "tit" ; 
      x + "es" => x + "ehet";
      x + "tu" => x + "nut" ; 
      x + ("i"|"t") => x + "et" ; 
      x + "ttö" => x + "töt" ; 
      x + "s" => x + "kset" ; 
      x + "e" => x + "eet" ;
      x + "nen" => x + "set";
      x + "in" => init sg + "met";
      _ => (regNoun sg).s ! Pl ! Nom 
      };
      Acc => case sg of { --would there be a way of avoiding this repetition since nom and acc inflections are the same? Todo for the future improvements.
      x + "akku" => x + "akut" ; 
      x + "attu" => x + "atut" ; 
      x + "appu" => x + "aput" ; 
      x + "akko" => x + "akot" ;
      x + "atto" => x + "atot" ; 
      x + "akki" => x + "akit" ; 
      x + "appi" => x + "apit" ; 
      x + "akka" => x + "akat" ; 
      x + "kki" => x + "kit" ; 
      x + "kka" => x + "kat" ; 
      x + "ppi" => x + "pit" ; 
      x + "tti" => x + "tit" ; 
      x + "ttö" => x + "töt" ;
      x + "et" => x + "ehet"; 
      x + "tu" => x + "nut" ; 
      x + ("i"|"t") => x + "et" ; 
      x + "s" => x + "kset" ; 
      x + "e" => x + "eet" ;
      x + "nen" => x + "set";
      x + "in" => init sg + "met" ;
      _ => (regNoun sg).s ! Pl ! Acc 
      };
      Gen => case sg of {
      x + ("li"|"ri"|"si"|"ni"|"pi"|"ka") => init sg + "ien";  
      x + "es" => x + "esten";
      x + "nen" => x + "sten" ;
      x + ("a"|"o") => x + "ojen" ;
      x + "i" => sg + "en";
      x + "ne" => x + "neiden";
      x + ("ö"| "u") => sg + "jen";
      x + "in" => init sg + "mien" ;
      x + "t" => x + "iden";
      _ => (regNoun sg).s ! Pl ! Gen 
      } ;
      Par => case sg of {
      x + "akku" => x + "akkuja" ; --kk -> k
      x + "attu" => x + "attuja" ; --tt -> t
      x + "appu" => x + "appuja" ; --pp -> p
      x + "akko" => x + "akkoja" ; --kk -> k
      x + "atto" => x + "attoja" ; --tt -> t
      x + "akki" => x + "akkeja" ; --kk -> k
      x + "appi" => x + "appeja" ; --pp -> p
      x + "akka" => x + "akkoja" ; --kk -> k
      x + "kki" => x + "kkeja" ; --kk -> k
      x + "kka" => x + "kkia" ; --kk -> k
      x + "ppi" => x + "ppeja" ; --pp -> p
      x + "tti" => x + "tteja" ; --tt -> i
      x + "ttö" => x + "töjä" ; --tt -> i
      x + "es" => x + "ehiss" + otherVowelHarmony(sg);
      x + "tu" => x + "tuja" ; 
      x + "t" => x + "it" + otherVowelHarmony(sg);
      x + "i" => x + "i" + vowelHarmony(last sg) ; 
      x + "s" => x + "ksia" ; --kasvis, kasvikset
      x + "e" => x + "eitä" ; -- virhe, virheet
      x + "nen" => x + "si" + otherVowelHarmony(sg);
      x + "in" => init sg + "miä";
      _ => (regNoun sg).s ! Pl ! Par 
      };
      Ine => case sg of {
      x + "akku" => x + "akuissa" ; --kk -> k
      x + "attu" => x + "atuissa" ; --tt -> t
      x + "appu" => x + "apuissa" ; --pp -> p
      x + "akko" => x + "akoissa" ; --kk -> k
      x + "atto" => x + "atoissa" ; --tt -> t
      x + "akki" => x + "akeissa" ; --kk -> k
      x + "appi" => x + "apeissa" ; --pp -> p
      x + "akka" => x + "akoissa" ; --kk -> k
      x + "kki" => x + "keissa" ; --kk -> k
      x + "kka" => x + "kissa" ; --kk -> k
      x + "ppi" => x + "peissa" ; --pp -> p
      x + "tti" => x + "teissa" ; --tt -> i
      x + "ttö" => x + "töissä" ; --tt -> i
      x + "tu" => x + "nuissa" ; 
      x + "es" => x + "ehist" + otherVowelHarmony(sg);
      x + "t" => x + "iss" + otherVowelHarmony(sg);
      x + "i" => x + "iss" + vowelHarmony(last sg) ; 
      x + "s" => x + "ksissa" ; 
      x + "e" => x + "eissä" ;
      x + "nen" => x + "siss" + otherVowelHarmony(sg);
      x + "in" => init sg + "miss" + otherVowelHarmony(sg);
      _ => (regNoun sg).s ! Pl ! Ine 
      };
      Ela => case sg of {
      x + "akku" => x + "akuista" ; --kk -> k
      x + "attu" => x + "atuista" ; --tt -> t
      x + "appu" => x + "apuista" ; --pp -> p
      x + "akko" => x + "akoista" ; --kk -> k
      x + "atto" => x + "atoista" ; --tt -> t
      x + "akki" => x + "akeista" ; --kk -> k
      x + "appi" => x + "apeista" ; --pp -> p
      x + "akka" => x + "akoista" ; --kk -> k
      x + "kki" => x + "keista" ; --kk -> k
      x + "kka" => x + "kista" ; --kk -> k
      x + "ppi" => x + "peista" ; --pp -> p
      x + "tti" => x + "teista" ; --tt -> i
      x + "ttö" => x + "töistä" ; --tt -> i
      x + "tu" => x + "nuista" ; 
      x + "es" => x + "ehist" + otherVowelHarmony(sg);
      x + "t" => x + "ist" + otherVowelHarmony(sg);
      x + "i" => x + "ist" + vowelHarmony(last sg) ; 
      x + "s" => x + "ksista" ; 
      x + "e" => x + "eistä" ;
      x + "nen" => x + "sist" + otherVowelHarmony(sg);
      x + "in" => init sg + "mist" + otherVowelHarmony(sg);
      _ => (regNoun sg).s ! Pl ! Ela 
      };
      Ill => case sg of {
      x + "akku" => x + "akkuihin" ; --kk -> k
      x + "attu" => x + "attuihin" ; --tt -> t
      x + "appu" => x + "appuihin" ; --pp -> p
      x + "akko" => x + "akkoihin" ; --kk -> k
      x + "atto" => x + "attoihin" ; --tt -> t
      x + "akki" => x + "akkeihin" ; --kk -> k
      x + "appi" => x + "appeihin" ; --pp -> p
      x + "akka" => x + "akkoihin" ; --kk -> k
      x + "kki" => x + "kkeihin" ; --kk -> k
      x + "kka" => x + "kkiin" ; --kk -> k
      x + "ppi" => x + "ppeihin" ; --pp -> p
      x + "tti" => x + "tteihin" ; --tt -> i
      x + "es" => x + "ehiin";
      x + "tu" => x + "tuihin" ; 
      x + "t" => x + "ihin";
      x + "ttö" => sg + "töihin" ; --tt -> i
      x + "i" => x + "in" ; 
      x + "s" => x + "ksiin" ; 
      x + "e" => x + "eisiin" ; -- virhe, virheet
      x + "uu" => sg + "uihin" ;
      x + "nen" => x + "siin" ;
      x + "in" => init sg + "miin" ;
      _ => (regNoun sg).s ! Pl ! Ill 
      };
      Ade => case sg of {
      x + "akku" => x + "akuilla" ; --kk -> k
      x + "attu" => x + "atuilla" ; --tt -> t
      x + "appu" => x + "apuilla" ; --pp -> p
      x + "akko" => x + "akoilla" ; --kk -> k
      x + "atto" => x + "atoilla" ; --tt -> t
      x + "akki" => x + "akeilla" ; --kk -> k
      x + "appi" => x + "apeilla" ; --pp -> p
      x + "akka" => x + "akoilla" ; --kk -> k
      x + "kki" => x + "keilla" ; --kk -> k
      x + "kka" => x + "killa" ; --kk -> k
      x + "ppi" => x + "peilla" ; --pp -> p
      x + "tti" => x + "teilla" ; --tt -> i
      x + "ttö" => x + "töillä" ; --tt -> i
      x + "tu" => x + "nuilla" ; 
      x + "es" => x + "ehill" + otherVowelHarmony(sg);
      x + "t" => x + "ill" + otherVowelHarmony(sg);
      x + "i" => x + "ill" + vowelHarmony(last sg) ; 
      x + "s" => x + "ksilla" ; 
      x + "e" => x + "eillä" ; -- veneillä
      x + "uu" => sg + "uilla" ;
      x + "nen" => x + "sill" + otherVowelHarmony(sg);
      x + "in" => init sg + "mill" + otherVowelHarmony(sg);
      _ => (regNoun sg).s ! Pl ! Ade 
      };
      Abl => case sg of {
      x + "akku" => x + "akuilta" ; --kk -> k
      x + "attu" => x + "atuilta" ; --tt -> t
      x + "appu" => x + "apuilta" ; --pp -> p
      x + "akko" => x + "akoilta" ; --kk -> k
      x + "atto" => x + "atoilta" ; --tt -> t
      x + "akki" => x + "akeilta" ; --kk -> k
      x + "appi" => x + "apeilta" ; --pp -> p
      x + "akka" => x + "akoilta" ; --kk -> k
      x + "kki" => x + "keilta" ; --kk -> k
      x + "kka" => x + "kilta" ; --kk -> k
      x + "ppi" => x + "peilta" ; --pp -> p
      x + "tti" => x + "teilta" ; --tt -> i
      x + "ttö" => x + "töiltä" ; --tt -> i
      x + "tu" => x + "nuilta" ; 
      x + "es" => x + "ehilt" + otherVowelHarmony(sg);
      x + "t" => x + "ilt" + otherVowelHarmony(sg);
      x + "i" => x + "ilt" + vowelHarmony(last sg) ; 
      x + "s" => x + "ksilta" ; 
      x + "e" => x + "eiltä" ; -- veneillä
      x + "uu" => sg + "uilta" ;
      x + "nen" => x + "silt" + otherVowelHarmony(sg);
      x + "in" => init sg + "milt" + otherVowelHarmony(sg);
      _ => (regNoun sg).s ! Pl ! Abl 
      };
      All => case sg of {
      x + "akku" => x + "akuille" ; --kk -> k
      x + "attu" => x + "atuille" ; --tt -> t
      x + "appu" => x + "apuille" ; --pp -> p
      x + "akko" => x + "akoille" ; --kk -> k
      x + "atto" => x + "atoille" ; --tt -> t
      x + "akki" => x + "akeille" ; --kk -> k
      x + "appi" => x + "apeille" ; --pp -> p
      x + "akka" => x + "akoille" ; --kk -> k
      x + "kki" => x + "keille" ; --kk -> k
      x + "kka" => x + "kille" ; --kk -> k
      x + "ppi" => x + "peille" ; --pp -> p
      x + "tti" => x + "teille" ; --tt -> i
      x + "es" => x + "ehille";
      x + "tu" => x + "nuille" ; 
      x + ("i"|"t") => x + "ille" ; --
      x + "ttö" => x + "töille" ; --tt -> i
      x + "s" => x + "ksille" ; 
      x + "e" => x + "eille" ; -- veneillä
      x + "uu" => sg + "uille" ;
      x + "nen" => x + "sille";
      x + "in" => init sg + "mille";
      _ => (regNoun sg).s ! Pl ! All 
      };
      Ess => case sg of {
      x + "akku" => x + "akkuina" ; --kk -> k
      x + "attu" => x + "attuina" ; --tt -> t
      x + "appu" => x + "appuina" ; --pp -> p
      x + "akko" => x + "akkoina" ; --kk -> k
      x + "atto" => x + "attoina" ; --tt -> t
      x + "akki" => x + "akkeina" ; --kk -> k
      x + "appi" => x + "appeina" ; --pp -> p
      x + "akka" => x + "akkoina" ; --kk -> k
      x + "kki" => x + "kkeina" ; --kk -> k
      x + "kka" => x + "kkina" ; --kk -> k
      x + "ppi" => x + "ppeina" ; --pp -> p
      x + "tti" => x + "teina" ; --tt -> i
      x + "ttö" => x + "töinä" ; --tt -> i
      x + "tu" => x + "tuina" ; 
      x + "es" => x + "ehin" + otherVowelHarmony(sg);
      x + "t" => x + "in" + otherVowelHarmony(sg);
      x + "i" => x + "in" + vowelHarmony(last sg) ; 
      x + "s" => x + "ina" ; 
      x + "e" => x + "einä" ; -- veneillä
      x + "uu" => sg + "uina" ;
      x + "nen" => x + "sin" + otherVowelHarmony(sg);
      x + "in" => init sg + "min" + otherVowelHarmony(sg);
      _ => (regNoun sg).s ! Pl ! Ess 
      };
      Tra => case sg of {
      x + "akku" => x + "akuiksi" ; --kk -> k
      x + "attu" => x + "atuiksi" ; --tt -> t
      x + "appu" => x + "apuiksi" ; --pp -> p
      x + "akko" => x + "akoiksi" ; --kk -> k
      x + "atto" => x + "atoiksi" ; --tt -> t
      x + "akki" => x + "akeiksi" ; --kk -> k
      x + "appi" => x + "apeiksi" ; --pp -> p
      x + "akka" => x + "akoiksi" ; --kk -> k
      x + "kki" => x + "keiksi" ; --kk -> k
      x + "kka" => x + "kiksi" ; --kk -> k
      x + "ppi" => x + "peiksi" ; --pp -> p
      x + "tti" => x + "teiksi" ; --tt -> i
      x + "ttö" => x + "töiksi" ; --tt -> i
      x + "es" => x + "ehiksi";
      x + "tu" => x + "nuiksi" ; 
      x + ("i"|"t") => x + "iksi" ; -- lapsiksi, tuliksi, kieliksi, oluiksi
      x + "s" => x + "ksiksi" ; 
      x + "e" => x + "eiksi" ; -- veneiksi
      x + "uu" => sg + "uiksi" ; --puiksi
      x + "nen" => x + "siksi";
      x + "in" => init sg + "miksi";
      _ => (regNoun sg).s ! Pl ! Tra 
      };
      Abe => case sg of {
      x + "akku" => x + "akuitta" ; --kk -> k
      x + "attu" => x + "atuitta" ; --tt -> t
      x + "appu" => x + "apuitta" ; --pp -> p
      x + "akko" => x + "akoitta" ; --kk -> k
      x + "atto" => x + "atoitta" ; --tt -> t
      x + "akki" => x + "akeitta" ; --kk -> k
      x + "appi" => x + "apeitta" ; --pp -> p
      x + "akka" => x + "akoitta" ; --kk -> k
      x + "kki" => x + "keitta" ; --kk -> k
      x + "kka" => x + "kitta" ; --kk -> k
      x + "ppi" => x + "peitta" ; --pp -> p
      x + "tti" => x + "teitta" ; --tt -> i
      x + "ttö" => x + "töittä" ; --tt -> i
      x + "tu" => x + "nuitta" ; 
      x + "es" => x + "ehitt" + otherVowelHarmony(sg);
      x + "t" => x + "itt" + otherVowelHarmony(sg);
      x + "i" => x + "itt" + vowelHarmony(last sg) ; --oluitta, lapsitta
      x + "s" => x + "sitta" ; 
      x + "e" => x + "eittä" ; -- veneillä
      x + "uu" => sg + "uitta" ;
      x + "nen" => x + "sitt" + otherVowelHarmony(sg);
      x + "in" => init sg + "mitt" + otherVowelHarmony(sg);
      _ => (regNoun sg).s ! Pl ! Abe
      }
    }
  }
};

-- ADJECTIVES : 
Adjective : Type = {s : Number => Case => Str} ;   

-- irregular adjective making oper
irregA : Str -> Str -> Adjective = --for irregular adjectives that have two forms in the lexicon
\sg,pl -> {
  s = table {
    Sg => table {
        Nom => sg ;
        Acc => case sg of {
          x + "ri" => x + "ren"; --very specific cases from our lexicon...has its limitations! Could have just given all the forms too. 
          x + "ni" => x + "nen";
          x + "is" => x + "iin";
          x + "si" => x + "den";
          x + "min" => x + "pimän"
        } ;
        Gen => case sg of {
          x + "ri" => x + "ren"; 
          x + "ni" => x + "nen";
          x + "is" => x + "iin";
          x + "si" => x + "den";
          x + "min" => x + "pimän"
        } ;
        Par => case sg of {
          x + "ri" => x + "rta"; 
          x + "ni" => x + "ntä";
          x + "is" => x + "ista";
          x + "si" => x + "tta";
          x + "in" => x + "intä"
        } ;
        Ine => case sg of {
          x + "ri" => x + "ressa"; 
          x + "ni" => x + "nessä";
          x + "is" => x + "iissa";
          x + "si" => x + "dessa";
          x + "min" => x + "pimässä"
        } ;
        Ela => case sg of {
          x + "ri" => x + "resta"; 
          x + "ni" => x + "nestä";
          x + "is" => x + "iista";
          x + "si" => x + "desta";
          x + "min" => x + "pimästä"
        } ;
        Ill => case sg of {
          x + "ri" => x + "reen"; 
          x + "ni" => x + "neen";
          x + "is" => x + "iiseen";
          x + "si" => x + "teen";
          x + "min" => x + "pimään"
        } ;
        Ade => case sg of {
          x + "ri" => x + "rella"; 
          x + "ni" => x + "nellä";
          x + "is" => x + "iilla";
          x + "si" => x + "della";
          x + "min" => x + "pimällä"
        } ;
        Abl => case sg of {
          x + "ri" => x + "relta"; 
          x + "ni" => x + "neltä";
          x + "is" => x + "iilta";
          x + "si" => x + "delta";
          x + "min" => x + "pimältä"
        } ;
        All => case sg of {
          x + "ri" => x + "relle"; 
          x + "ni" => x + "nelle";
          x + "is" => x + "iille";
          x + "si" => x + "delle";
          x + "min" => x + "pimälle"
        } ;
        Ess => case sg of {
          x + "ri" => x + "rena"; 
          x + "ni" => x + "nenä";
          x + "is" => x + "iina";
          x + "si" => x + "tena";
          x + "min" => x + "pimänä"
        } ;
        Tra => case sg of {
          x + "ri" => x + "reksi"; 
          x + "ni" => x + "neksi";
          x + "is" => x + "iiksi";
          x + "si" => x + "deksi";
          x + "min" => x + "pimäksi"
        } ;
        Abe => case sg of {
          x + "ri" => x + "retta"; 
          x + "ni" => x + "nettä";
          x + "is" => x + "iitta";
          x + "si" => x + "detta";
          x + "min" => x + "pimättä"
        } 
    };
    Pl => table {
        Nom => pl ;
        Acc => case sg of {
          x + "ri" => x + "ret"; 
          x + "ni" => x + "net";
          x + "is" => x + "iit";
          x + "si" => x + "det";
          x + "min" => x + "pimät"
        } ;
        Gen => case sg of {
          x + "ri" => x + "ret"; 
          x + "ni" => x + "net";
          x + "is" => x + "iit";
          x + "si" => x + "det";
          x + "min" => x + "pimät"
        } ;
        Par => case sg of {
          x + "ri" => x + "ria"; 
          x + "ni" => x + "niä";
          x + "is" => x + "iita";
          x + "si" => x + "sia";
          x + "min" => x + "pimiä"
        } ;
        Ine => case sg of {
          x + "ri" => x + "rissa"; 
          x + "ni" => x + "nissä";
          x + "is" => x + "iissa";
          x + "si" => x + "sissa";
          x + "min" => x + "pimissä"
        } ;
        Ela => case sg of {
          x + "ri" => x + "rista"; 
          x + "ni" => x + "nistä";
          x + "is" => x + "iista";
          x + "si" => x + "sista";
          x + "min" => x + "pimistä"
        } ;
        Ill => case sg of {
          x + "ri" => x + "riin"; 
          x + "ni" => x + "niin";
          x + "is" => x + "iisiin";
          x + "si" => x + "siin";
          x + "min" => x + "pimiin"
        } ;
        Ade => case sg of {
          x + "ri" => x + "rilla"; 
          x + "ni" => x + "nillä";
          x + "is" => x + "iilla";
          x + "si" => x + "silla";
          x + "min" => x + "pimillä"
        } ;
        Abl => case sg of {
          x + "ri" => x + "rilta"; 
          x + "ni" => x + "niltä";
          x + "is" => x + "iilta";
          x + "si" => x + "silta";
          x + "min" => x + "pimiltä"
        } ;
        All => case sg of {
          x + "ri" => x + "rille"; 
          x + "ni" => x + "nille";
          x + "is" => x + "iille";
          x + "si" => x + "sille";
          x + "min" => x + "pimille"
        } ;
        Ess => case sg of {
          x + "ri" => x + "rina"; 
          x + "ni" => x + "ninä";
          x + "is" => x + "iina";
          x + "si" => x + "sina";
          x + "min" => x + "piminä"
        } ;
        Tra => case sg of {
          x + "ri" => x + "riksi"; 
          x + "ni" => x + "niksi";
          x + "is" => x + "iiksi";
          x + "si" => x + "siksi";
          x + "min" => x + "pimiksi"
        } ;
        Abe => case sg of {
          x + "ri" => x + "ritta"; 
          x + "ni" => x + "nittä";
          x + "is" => x + "iitta";
          x + "si" => x + "sitta";
          x + "min" => x + "pimittä"
        } 
    }
  }
};

-- ADJECTIVE - MAKING OPER !
mkAdjective : Str -> Adjective = --luckily most of the adjectives in the lexicon are relatively regular so we can use the regNoun oper for most of them :-)
  \sg -> {
  s = table {
    Sg => table {
            Nom => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Nom;
              _ => (regNoun sg).s ! Sg ! Nom 
            };
            Acc => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Acc;
              _ => (regNoun sg).s ! Sg ! Acc 
            };
            Gen => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Gen;
              _ => (regNoun sg).s ! Sg ! Gen 
            };
            Par => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Par;
              _ => (regNoun sg).s ! Sg ! Par 
            };
            Ine => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Ine;
              _ => (regNoun sg).s ! Sg ! Ine 
            };
            Ela => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Ela;
              _ => (regNoun sg).s ! Sg ! Ela 
            };
            Ill => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Ill;
              _ => (regNoun sg).s ! Sg ! Ill 
            }; 
            Ade => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Ade;
              _ => (regNoun sg).s ! Sg ! Ade 
            };
            Abl => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Abl;
              _ => (regNoun sg).s ! Sg ! Abl 
            };
            All => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! All;
              _ => (regNoun sg).s ! Sg ! All 
            };
            Ess => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Ess;
              _ => (regNoun sg).s ! Sg ! Ess 
            };
            Tra => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Tra;
              _ => (regNoun sg).s ! Sg ! Tra 
            };
            Abe => case sg of  {
              x + "nen" => (smartNoun sg).s ! Sg ! Abe;
              _ => (regNoun sg).s ! Sg ! Abe 
            }
    }; 
    Pl => table {
             Nom => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Nom;
              _ => (regNoun sg).s ! Pl ! Nom 
            };
            Acc => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Acc;
              _ => (regNoun sg).s ! Pl ! Acc 
            };
            Gen => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Gen;
              _ => (regNoun sg).s ! Pl ! Gen 
            };
            Par => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Par;
              _ => (regNoun sg).s ! Pl ! Par 
            };
            Ine => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Ine;
              _ => (regNoun sg).s ! Pl ! Ine 
            };
            Ela => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Ela;
              _ => (regNoun sg).s ! Pl ! Ela 
            };
            Ill => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Ill;
              _ => (regNoun sg).s ! Pl ! Ill 
            }; 
            Ade => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Ade;
              _ => (regNoun sg).s ! Pl ! Ade 
            };
            Abl => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Abl;
              _ => (regNoun sg).s ! Pl ! Abl 
            };
            All => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! All;
              _ => (regNoun sg).s ! Pl ! All 
            };
            Ess => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Ess;
              _ => (regNoun sg).s ! Pl ! Ess 
            };
            Tra => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Tra;
              _ => (regNoun sg).s ! Pl ! Tra 
            };
            Abe => case sg of  {
              x + "nen" => (smartNoun sg).s ! Pl ! Abe;
              _ => (regNoun sg).s ! Pl ! Abe 
            }
    } 
  } 
} ;

-- VERBS 
Verb : Type = {s : Number => Person => Tense => Str} ;

 -- we need to take into consideration that the past participle is different in singular (persg) and plural personas (perpl)!  

-- VERB-MAKING OPER:
mkVerb : (stem, persg, perpl : Str) -> Verb
  = \stem, persg, perpl -> {
    s = table {
      Sg => table {
        Per1 => table {
            Pres => case stem of {
            "o" => "olen";
              _ => stem + "n" };
            Per => "olen_" + persg ; --how to add space? Olen is an auxiliary (like English have)
            Pkp => "olin_" + persg 
      } ;
        Per2 => table {
            Pres => stem + "t" ;
            Per => "olet_" + persg ;
            Pkp => "olit_" + persg
        } ;
        Per3 => table {  -- 3rd person requires some more pattern matching, also in plural (but not as much)
            Pres => case stem of {    --using an oper to remove repetition at least a bit, probably there would have been a more optimal way too (todo in the future)
              "o" => "on";
              l + "ue" => l + "ukee";
              ymmär + "rä" => ymmär + "t" + doubleVowel(stem);
              ta + "pa" => ta + "pp" + doubleVowel(stem) ;
              pel + "aa" => stem;
              luk + "ee" => luk + doubleVowel(stem); 
              löy + "dä" => löy + "t" + doubleVowel(stem);
              odo + "ta" => odo + "tt" + doubleVowel(stem);
              j + "uo" => j + "uo";
              nu + "ku" => nu + "kk" + doubleVowel(stem) ;
              men + "e" => men + doubleVowel(stem);   
              el + "ä" => el + doubleVowel(stem);   
              rik + "o" => rik + "k" + doubleVowel(stem);
              rakast + "a" => rakast + doubleVowel(stem);
              _ => stem
            }  ;
            Per => "on_" + persg ;
            Pkp => "oli_" + persg
      } 
      };
      Pl => table {
        Per1 => table {
            Pres => case stem of {
              "o" => "olemme";
               _ => stem + "mme" 
              };
            Per => "olemme_" + perpl ;
            Pkp => "olimme_" + perpl
        } ;
        Per2 => table {
            Pres => case stem of {
              "o" => "olette";
               _ => stem + "tte" 
            };
            Per => "olette_" +  perpl ;
            Pkp => "olitte_" + perpl
        } ;
        Per3 => table {
            Pres => case stem of {
              ta + "pa" => ta + "ppavat" ;
              rakas + "ta" => rakas + "tavat";
              ri + "ko" => ri + "kkovat";
              nu + "ku" => nu + "kkuvat";
              tap + "a" => tap + "aavat";
              _ => stem + "v" + otherVowelHarmony(stem) + "t" 
              };
            Per => "ovat_" + perpl ;
            Pkp => "olivat_" + perpl
          } 
      }
    }
} ;

-- this one will only be used with a couple of the verbs --> ymmärtää-nähdä-juosta-lukea-löytää
  irregVerb : Str -> Str -> Str -> Str -> Verb 
  = \inf, stem, persg, perpl -> 
      mkVerb stem persg perpl ;

-- SMART - VERB OPER
  -- we will stick to the present and present past and past perfect tenses
  -- we need to take into consideration that the past participle is different in singular and plural personas
  -- third person conjugation 'issues' will be sorted in mkVerb
  smartVerb : Str -> Verb = \inf -> 
    case inf of {
   x + ("ata"|"ätä") => mkVerb (x + verbHelpVowelHarmony(inf) + verbHelpVowelHarmony(inf)) (x + verbHelpVowelHarmony(inf) + "nnut") (x + verbHelpVowelHarmony(inf) + "nneet") ;       --verbtype  4 ends in ata ätä, will go through changes in the stem   
   x + "tt" + y => mkVerb (x + "t" + verbHelpVowelHarmony(init inf)) (init inf + "nut") (init inf + "neet");  --verbs ending in two vowels that require variation in the consonant stem  -opettaa verbtype 1
   x + "pp" + y => mkVerb (x + "p" + verbHelpVowelHarmony(init inf)) (init inf + "nut") (init inf + "neet");  --verbs ending in two vowels that require variation in the consonant stem  -tappaa verbtype 1
   x + "kk" + y => mkVerb (x + "k" + verbHelpVowelHarmony(init inf)) (init inf + "nut") (init inf + "neet");  --verbs ending in two vowels that require variation in the consonant stem  -rikkoa verbtype 1
   rakast + ("aa"| "öö"| "uu"| "oo" | "ää"| "ea"| "yy") => mkVerb (init inf) (rakast + verbHelpVowelHarmony(last inf) + "n" + pastParticipleVowelHarmony(last inf) + "t") (rakast + verbHelpVowelHarmony(last inf) + "neet") ;
   tul + ("la" | "lä" | "nä" | "rä" | "ta" | "tä")  => mkVerb (tul + "e") (tul + ekstraKonsonantti(init inf) + pastParticipleVowelHarmony(last inf) + "t") (tul + ekstraKonsonantti(init inf) + "eet") ;   --verbtype 3 - ends in -la, -lä; -na, -nä; -ra, -rä; -sta, -stä
   juo  +  ("da" | "dä") =>  mkVerb juo (juo + "n" + pastParticipleVowelHarmony(last inf) + "t") (juo + "neet")   -- verbtype 2. ends in -da or -dä. juoda, syödä, uida, nähdä, tehdä
   --pela  +  ("a" | "ä")  => regVerb inf     -- perusmuodon lopussa on a tai ä.  rikkoa, lukea, ymmärtää, mennä, ostaa, löytää, tappaa. elää, rakastaa, nukkua, opettaa, matkustaa, odottaa
   } ;
 
  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = irregVerb "olla" "o" "ollut" "olleet" ; ---s to be generalized

--- VERB AGREEMENT FUNCTION - HOW TO IMPLEMENT THIS IN THE FUTURE?
--  agr2vform : Agreement -> Verbform = \a -> case a of {
--   NAgr Sg Per1 Pres => \v -> v.s ! Sg ! Per1 ! Pres ;
--   NAgr Sg Per2 Pres => \v -> v.s ! Sg ! Per2 ! Pres ;
--   NAgr Sg Per3 Pres => \v -> v.s ! Sg ! Per3 ! Pres ;
--   NAgr Pl Per1 Pres => \v -> v.s ! Pl ! Per1 ! Pres ;
--   NAgr Pl Per2 Pres => \v -> v.s ! Pl ! Per2 ! Pres ;
--   NAgr Pl Per3 Pres => \v -> v.s ! Pl ! Per3 ! Pres ;

--   NAgr Sg Per1 Per => \v -> v.s ! Sg ! Per1 ! Per ;
--   NAgr Sg Per2 Per => \v -> v.s ! Sg ! Per2 ! Per ;
--   NAgr Sg Per3 Per => \v -> v.s ! Sg ! Per3 ! Per ;
--   NAgr Pl Per1 Per => \v -> v.s ! Pl ! Per1 ! Per ;
--   NAgr Pl Per2 Per => \v -> v.s ! Pl ! Per2 ! Per ;
--   NAgr Pl Per3 Per => \v -> v.s ! Pl ! Per3 ! Per ;

--   NAgr Sg Per1 Pkp => \v -> v.s ! Sg ! Per1 ! Pkp ;
--   NAgr Sg Per2 Pkp => \v -> v.s ! Sg ! Per2 ! Pkp ;
--   NAgr Sg Per3 Pkp => \v -> v.s ! Sg ! Per3 ! Pkp ;
--   NAgr Pl Per1 Pkp => \v -> v.s ! Pl ! Per1 ! Pkp ;
--   NAgr Pl Per2 Pkp => \v -> v.s ! Pl ! Per2 ! Pkp ;
--   NAgr Pl Per3 Pkp => \v -> v.s ! Pl ! Per3 ! Pkp
-- } ;


};

