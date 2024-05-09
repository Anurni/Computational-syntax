resource MicroResFi = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Gen | Par | Ine | Ela | Ill | Ade | Abl | All | Ess | Tra | Abe ; --left out two cases: instruktiivi and komitatiivi, lets see whether or not to include them later
  Tense = Pres | Per | Pkp ;
  Person = Per1 | Per2 | Per3 ;


  Agreement = Agr Number Case ; -- adjectives agree with nouns in number and case
  AgreementVerbs = VAgr Number Person Tense ; --verbs agree with 


oper
-- declaring the noun type in Finnish
  Noun : Type = {s : Number => Case => Str} ;

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
  otherVowelHarmony : Str -> Str ;   --let's see if we can replace the original vowel harmony with this logic
  otherVowelHarmony = \character -> case character of {
    x + ("a"|"o"|"u") + y => "a";
    _ => "ä"
  };

  illativeVowelHarmony : Str -> Str ;
  illativeVowelHarmony = \character -> case character of {
    x + "o" => "oo";
    x + "u" => "uu";
    x + "a" => "aa";
    x + ("i"|"e") => "ee";
    x + "ä" => "ää"
  };

  pastParticipleVowelHarmony : Str -> Str ;
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


-- for nouns that don't have the kpt rule ?
  mkNoun : Str -> Str -> Noun = \sg,pl -> {  
    s = table {
        Sg => table {
            Nom => sg ; -- READY
            Acc => sg + "n" ; -- READY
            Gen => sg + "n" ; -- READY
            Par => case sg of { --probably not ready...
              x + ("ää"|"aa"|"uu"|"s"|"r"|"n"|"t") => sg + "t" + vowelHarmony(last sg) ;
              x + "e" => sg + "tt" + otherVowelHarmony(sg) ;  
              x + ("a"|"o"|"u"|"y"|"i"|"ä"|"ö") => sg + vowelHarmony(last sg)  
            } ;
            Ine => sg + "ss" + vowelHarmony(last sg);
            Ela => sg + "st" + vowelHarmony(last sg);
            Ill => case sg of { --taloon vs. laivaan vs. halpahalliin according to the 
            x + "uu" => sg + "hun";
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
            Nom => pl ; -- READY
            Acc => pl ; -- READY MAYBE??
            Gen => case sg of {  
            x + ("ka"| "i"|"ä"|"na"|"ra") => init sg + "ien" ; --poika poikien, tuli tulien viini viinien lehmä lehmien...
            x + ("o"|"a") => x + "ojen" ;
            x + "uu" => x + "uiden";
            ("r"| "n" | "t"| "s") => "ten"  --miesten, tytärten 
            } ;
            Par => case sg of {  
            x + ("ka"|"na"|"ra") => init sg + "ia" ; --poika poikia, tuli tulia
            x + ("o"|"a") => init sg + "oja" ;
            x + "uu" => x + "uita";
            x + "ä" => init sg + "iä" ;
            x + "i" => init sg + "ejä"
            } ;
            Ine => case sg of {
            x + "to" => sg + "iss" + vowelHarmony(last sg);
            x + ("i"|"ä"|"na"|"uu") => init sg + "iss" + vowelHarmony(last sg) ;
            _ => init sg + "oiss" + vowelHarmony(last sg)
            };
            Ela => case sg of {
            x + "to" => sg + "ist" + vowelHarmony(last sg);
            x + ("i"|"ä"|"na"|"uu") => init sg + "ist" + vowelHarmony(last sg) ;
            _ => init sg + "oist" + vowelHarmony(last sg)
            };
            Ill => case sg of {  --vowel harmony needs to be taken into account
            x + "to" => x + "ihin" ; --back vowels
            x + ("a" | "sa"| "pi"| "va") => init sg + "oihin" ; --front vowels
            _ => init sg + "iin"
            };
            Ade => case sg of {
              x + ("na"|"vä"|"sa"|"ra"|"rä"|"nen") => init sg + "ill" + vowelHarmony(last sg);
              x + ("va") => init sg + "oill" + vowelHarmony(last sg);
              x + "a" => init sg + "oill" + vowelHarmony(last sg);
              _ => sg + "ill" + vowelHarmony(last sg)
            };
            Abl => case sg of {
              x + ("na"|"vä"|"sa"|"ra"|"rä"|"nen") => init sg + "ilt" + vowelHarmony(last sg);
              x + ("va") => init sg + "oilt" + vowelHarmony(last sg);
              x + "a" => init sg + "oilt" + vowelHarmony(last sg);
              _ => sg + "ilt" + vowelHarmony(last sg)
            };
            All => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oille";
              x + ("rä"|"vä") => init sg + "ille";
              x + "a" => init sg + "oill" + vowelHarmony(last sg);
              _ => sg + "ille"
            };
            Ess => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oin" + vowelHarmony(last sg);
              x + ("rä"|"vä") => init sg + "in" + vowelHarmony(last sg);
              x + "a" => init sg + "oin" + vowelHarmony(last sg);
              _ => sg + "in" + vowelHarmony(last sg)
            };
            Tra => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oiksi";
              x + ("rä"|"vä") => init sg + "iksi";
              x + "a" => init sg + "oiksi";
              _ => sg + "iksi"
            };
            Abe => case sg of {
              x + ("na"|"va"|"sa") => init sg + "oitt" + vowelHarmony(last sg);
              x + ("rä"|"vä") => init sg + "itt" + vowelHarmony(last sg);
              x + "a" => init sg + "oitt" + vowelHarmony(last sg);
              _ => sg + "itt" + vowelHarmony(last sg)
            }
        }
    }
} ;

  -- regular paradigm for regular cases like talo talot, vauva vauvat, juna junat, laiva laivat, viini viinit, koira koirat....
  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "t") ;

  -- smart paradigms (trying to implement at least the lemma-modifying KPT (strong-weak variation) rule for now) to cover words like tyttö tytöt, kukka kukat, kielioppi kieliopit...
  --trying to implement noun case inflection for kpt nouns, trtying to cover similar words that are not necessarily even in the lexicon
  -- also added pattern matching for adjectives
smartNoun : Str -> Noun = \sg -> {
  s = table {
    Sg => table {
      Nom => sg ;
      Acc => case sg of {
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
        x + "i" => x + "en" ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "ksen" ; --kasvis, kasvikset
        x + "e" => x + "een" ; -- virhe, virheet
        x + "nen" => x + "sen"
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
        x + "i" => x + "en" ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "ksen" ; --kasvis, kasvikset
        x + "e" => x + "een" ; -- tietokoneen, veneen, 
        x + "nen" => x + "sen"
      };
      Par => case sg of {
        x + "nen" => x + "st" + otherVowelHarmony(sg);
        x + "ppi" => sg + "a";
        x + "n" => x + "tä";
        x + "e" => x + "että";
        x + ("i"|"t"|"s") => x + "t" + vowelHarmony(last sg);
        x + "a" => x + "aa";
        x + "u" => x + "ua";
        x + "o" => x + "oa";
        x + "ö" => x + "öä";
        x + "ä" => x + "ää" 
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
        x + "i" => x + "ess" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "ksessa" ; --kasvis, kasvikset
        x + "e" => x + "eessä" ;  -- virhe, virheet
        x + "nen" => x + "sess" + otherVowelHarmony(sg)
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
        x + "i" => x + "est" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "ksesta" ; --kasvis, kasvikset
        x + "e" => x + "eestä" ; -- virhe, virheet
        x + "nen" => x + "sest" + otherVowelHarmony(sg)
      } ;
      Ill => case sg of {
        x + ("li"|"ri"|"si") => init sg + "een";
        x + "nen" => x + "seen";
        x + "a" => x + "aan" ;
        x + "u" => x + "un" ;
        x + "o" => x + "on" ;
        x + "s" => x + "en" ;
        x + "e" => x + "seen";
        x + "i" => x + "iin"
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
        x + "i" => x + "ell" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "ksella" ; --kasvis, kasvikset
        x + "e" => x + "eellä" ; -- virhe, virheet
        x + "nen" => x + "sell" + otherVowelHarmony(sg)
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
        x + "i" => x + "elt" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "kselta" ; --kasvis, kasvikset
        x + "e" => x + "eeltä" ; -- virhe, virheet
        x + "nen" => x + "selt" + otherVowelHarmony(sg)
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
        x + "i" => x + "elle" ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "kselle" ; --kasvis, kasvikset
        x + "e" => x + "eelle" ; -- virhe, virheet
        x + "nen" => x + "selle"
      } ;
      Ess => case sg of {
        x + "nen" => x + "sen" + otherVowelHarmony(sg) ;
        x + ("pi"|"a"|"o"|"u") => sg + "na" ;
        x + ("ä"|"ö"|"i"|"e") => x + "enä"
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
        x + "i" => x + "eksi" ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "kseksi" ; --kasvis, kasvikset
        x + "e" => x + "eeksi" ; -- virhe, virheet
        x + "nen" => x + "seksi" 
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
        x + "i" => x + "ett" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
        x + "s" => x + "ksetta" ; --kasvis, kasvikset
        x + "e" => x + "eettä" ; -- virhe, virheet
        x + "nen" => x + "sett" + otherVowelHarmony(sg)
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
      x + "i" => x + "et" ; 
      x + "s" => x + "kset" ; 
      x + "e" => x + "eet" ;
      x + "nen" => x + "set"
      };
      Acc => case sg of { --would there be a way of avoiding this repetition since nom and acc are the same?
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
      x + "i" => x + "et" ; 
      x + "s" => x + "kset" ; 
      x + "e" => x + "eet" ;
      x + "nen" => x + "set" 
      };
      Gen => case sg of {
      x + ("li"|"ri"|"si"|"ni"|"pi"|"ka") => init sg + "ien";  
      x + "nen" => x + "sten" ;
      x + ("a"|"o") => x + "ojen" ;
      x + "i" => sg + "en";
      x + ("ö"| "u") => sg + "jen" 
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
      x + "i" => x + "i" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ksia" ; --kasvis, kasvikset
      x + "e" => x + "eitä" ; -- virhe, virheet
      x + "nen" => x + "si" + otherVowelHarmony(sg)
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
      x + "i" => x + "iss" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ksissa" ; --kasvis, kasvikset
      x + "e" => x + "eissä" ;
      x + "nen" => x + "siss" + otherVowelHarmony(sg)
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
      x + "i" => x + "ist" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ksista" ; --kasvis, kasvikset
      x + "e" => x + "eistä" ;
      x + "nen" => x + "sist" + otherVowelHarmony(sg)
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
      x + "i" => x + "iin" ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ksiin" ; --kasvis, kasvikset
      x + "e" => x + "eisiin" ; -- virhe, virheet
      x + "uu" => sg + "uihin" ;
      x + "nen" => x + "siin"  
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
      x + "i" => x + "ill" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ksilla" ; --kasvis, kasvikset
      x + "e" => x + "eillä" ; -- veneillä
      x + "uu" => sg + "uilla" ;
      x + "nen" => x + "sill" + otherVowelHarmony(sg)
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
      x + "i" => x + "ilt" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ksilta" ; --kasvis, kasvikset
      x + "e" => x + "eiltä" ; -- veneillä
      x + "uu" => sg + "uilta" ;
      x + "nen" => x + "silt" + otherVowelHarmony(sg)
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
      x + "i" => x + "ille" ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ksille" ; --kasvis, kasvikset
      x + "e" => x + "eille" ; -- veneillä
      x + "uu" => sg + "uille" ;
      x + "nen" => x + "sille"
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
      x + "i" => x + "in" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "ina" ; --kasvis, kasvikset
      x + "e" => x + "einä" ; -- veneillä
      x + "uu" => sg + "uina" ;
      x + "nen" => x + "sin" + otherVowelHarmony(sg)
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
      x + "i" => x + "iksi" ; -- lapsiksi, tuliksi, kieliksi, virheellinen: viiniksi (viineiksi)
      x + "s" => x + "ksiksi" ; -- kasviksiksi
      x + "e" => x + "eiksi" ; -- veneiksi
      x + "uu" => sg + "uiksi" ; --puiksi
      x + "nen" => x + "siksi"
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
      x + "i" => x + "itt" + vowelHarmony(last sg) ; --some nouns ending in i in singular (like lapsi, vesi,) we have a problem here tho bc some of these are regular (like muki), need to check this
      x + "s" => x + "sitta" ; --kasvis, kasvikset
      x + "e" => x + "eittä" ; -- veneillä
      x + "uu" => sg + "uitta" ;
      x + "nen" => x + "sitt" + otherVowelHarmony(sg)
      }
    }
  }
};

Adjective : Type = {s : Number => Case => Str} ;   

irregA : Str -> Str -> Adjective = --for irregular adjectives that have two forms in the lexicon
\sg,pl -> {
  s = table {
    Sg => table {
        Nom => sg ;
        Acc => case sg of {
          x + "ri" => x + "ren"; --very specific cases from our lexicon...probably not the most optimal way. Could have just given all the forms too.
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
          x + "ri" => x + "ret"; --very specific cases from our lexicon...probably not the most optimal way. Could have just given all the forms too.
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

mkA : Str -> Adjective = --luckily most of the adjectives in the lexicon are relatively regular so we can use the regNoun oper for most of them
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

Verb : Type = {s : Number => Person => Tense => Str} ;

 -- we need to take into consideration that the past participle is different in singular (persg) and plural personas (perpl)  

mkVerb : (stem, persg, perpl : Str) -> Verb
  = \stem, persg, perpl -> {
    s = table {
      Sg => table {
        Per1 => table {
            Pres => stem + "n" ;
            Per => "olen" + "_" + persg ; --how to add space?
            Pkp => "olin" + "_" + persg 
      } ;
        Per2 => table {
            Pres => stem + "t" ;
            Per => "olet" + "_" + persg ;
            Pkp => "olit" + "_" + persg
        } ;
        Per3 => table {
            Pres => stem  ;
            Per => "on" + "_" + persg ;
            Pkp => "oli" + "_" + persg
      } 
      };
      Pl => table {
        Per1 => table {
            Pres => stem + "mme" ;
            Per => "olemme" + "_" + perpl ;
            Pkp => "olimme" + "_" + perpl
        } ;
        Per2 => table {
            Pres => stem + "tte" ;
            Per => "olette" + "_" +  perpl ;
            Pkp => "olitte" + "_" + perpl
        } ;
        Per3 => table {
            Pres => stem + "vat" ;
            Per => "ovat" + "_" + perpl ;
            Pkp => "olivat" + "_" + perpl
          } 
      }
    }
} ;

-- this one will only be used with a couple of the verbs ? could not get this to work
  irregVerb : Str -> Str -> Str -> Str -> Verb 
  = \inf, stem, persg, perpl -> 
      mkVerb stem persg perpl ;

  -- regular verbs with predictable variations
  -- we will stick to the present and present past and past perfect tenses
  -- we need to take into consideration that the past participle is different in singular and plural personas
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

  -- normal irregular verbs e.g. drink,drank,drunk
  --irregVerb : (inf,stem, persg, perpl : Str) -> Verb =
  --  \inf, stem,persg,perpl ->
  --    let verb = smartVerb inf
  --    in mkVerb stem persg perpl ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  --Verb2 : Type = Verb ** {c : Str} ;

  --be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  --agr2vform : Agreement -> VForm = \a -> case a of {
  --  Agr Sg => PresSg3 ;
  --  Agr Pl => Inf
  --  } ;

};

