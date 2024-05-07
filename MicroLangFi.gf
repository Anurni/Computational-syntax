--# -path=.:../abstract
concrete MicroLangFi of MicroLang = open MicroResFi, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Str} ; ---s special case of Mini
    Comp = {s : Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    --Det = {s : Str ; n : Number} ; --not sure about this one
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl
      } ;
      
    UseV v = {
      verb = v ;
      compl = [] ;
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = comp.s
      } ;
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    --DetCN det cn = {
    --  s = \\c => det.s ++ cn.s ! det.n ;
    --  a = Agr det.n ;
    --  } ;
      
    UsePron p = p ;
            
    --a_Det = {s = pre {"a"|"e"|"i"|"o" => "an" ; _ => "a"} ; n = Sg} ; --- a/an can get wrong
    --aPl_Det = {s = "" ; n = Pl} ;
    --the_Det = {s = "the" ; n = Sg} ;
    --thePl_Det = {s = "the" ; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    --in_Prep = {s = "in"} ;
    --on_Prep = {s = "on"} ;
    --with_Prep = {s = "with"} ;

    minä_Pron = {
      s = table {Nom => "minä" ; Acc => "minut"} ;
      a = Agr Sg ;
      } ;
    sinä_Pron = {
      s = table {Nom => "sinä" ; Acc => "sinut"} ;
      a = Agr Sg ;
      } ;
    hän_Pron = {
      s = table {Nom => "hän" ; Acc => "hänet"} ;
      a = Agr Sg ;
      } ;
    me_Pron = {
      s = table {Nom => "me" ; Acc => "meidät"} ;
      a = Agr Pl ;
      } ;
    te_Pron = {
      s = table {Nom => "te" ; Acc => "teidät"} ;
      a = Agr Pl ;
      } ;
    he_Pron = {
      s = table {Nom => "he" ; Acc => "heidät"} ;
      a = Agr Pl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "jo" ;
lin animal_N = mkN "eläin" ;
lin apple_N = mkN "omena" ; --REG
lin baby_N = mkN "vauva" ; --REG
lin bad_A = mkA "paha" ;
lin beer_N = mkN "olut" ;
lin big_A = mkA "iso" ;
lin bike_N = mkN "polkupyörä" ; --REG
lin bird_N = mkN "lintu" ;
lin black_A = mkA "musta" ;
lin blood_N = mkN "veri" ;
lin blue_A = mkA "sininen" ;
lin boat_N = mkN "vene" ;
lin book_N = mkN "kirja" ;
lin boy_N = mkN "poika" ;
lin bread_N = mkN "leipä" ;
lin break_V2 = mkV2 "rikkoa" ; --(mkV "break" "broke" "broken") ;
lin buy_V2 = mkV2 "ostaa" ; --(mkV "buy" "bought" "bought") ;
lin car_N = mkN "auto" ;
lin cat_N = mkN "kissa" ;
lin child_N = mkN "lapsi" ;
lin city_N = mkN "kaupunki" "kaupungit" ;
lin clean_A = mkA "siisti" ;
lin clever_A = mkA "fiksu" ;
lin cloud_N = mkN "pilvi" ;
lin cold_A = mkA "kylmä" ;
lin come_V = mkV "tulla" ;
lin computer_N = mkN "tietokone" ;
lin cow_N = mkN "lehmä" ;
lin dirty_A = mkA "likainen" ;
lin dog_N = mkN "koira" ;
lin drink_V2 = mkV2 "juoda" ; --(mkV "drink" "drank" "drunk") ;
lin eat_V2 = mkV2 "syödä" ;   --(mkV "eat" "ate" "eaten") ;
lin find_V2 = mkV2 "löytää" ; --(mkV "find" "found" "found") ;
lin fire_N = mkN "tuli" ;
lin fish_N = mkN "kala" ;
lin flower_N = mkN "kukka" ;
lin friend_N = mkN "ystävä" ;
lin girl_N = mkN "tyttö" ;
lin good_A = mkA "hyvä" ;
lin go_V = mkV "mennä" ;      --"go" "went" "gone" ;
lin grammar_N = mkN "kielioppi" ;
lin green_A = mkA "vihreä" ;
lin heavy_A = mkA "painava" ;
lin horse_N = mkN "hevonen" ;
lin hot_A = mkA "kuuma" ;
lin house_N = mkN "talo" ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "hypätä" ;
lin kill_V2 = mkV2 "tappaa" ;
-- lin know_VS = mkVS (mkV "know" "knew" "known") ;
lin language_N = mkN "kieli" ;
lin live_V = mkV "elää" ;
lin love_V2 = mkV2 "rakastaa" ; --(mkV "love") ;
lin man_N = mkN "mies" "miehet" ;
lin milk_N = mkN "maito" ;
lin music_N = mkN "musiikki" ;
lin new_A = mkA "uusi" "uudet" ;
lin now_Adv = mkAdv "nyt" ;
lin old_A = mkA "vanha" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "pelata" ;
lin read_V2 = mkV2 "lukea" ; --(mkV "read" "read" "read") ;
lin ready_A = mkA "valmis" "valmiit" ;
lin red_A = mkA "punainen" ;
lin river_N = mkN "joki" ;
lin run_V = mkV "juosta" ;
lin sea_N = mkN "meri" ;
lin see_V2 = mkV2 "nähdä" ; --(mkV "see" "saw" "seen") ;
lin ship_N = mkN "laiva" ;
lin sleep_V = mkV "nukkua" ;
lin small_A = mkA "pieni" "pienet" ;
lin star_N = mkN "tähti" ;
lin swim_V = mkV "uida" ;
lin teach_V2 = mkV2  "opettaa" ;     --(mkV "teach" "taught" "taught") ;
lin train_N = mkN "juna" ;
lin travel_V = mkV "matkustaa" ;
lin tree_N = mkN "puu" ;
lin understand_V2 = mkV2 "ymmärtää" ;       --(mkV "understand" "understood" "understood") ;
lin wait_V2 = mkV2 "odottaa" ;
lin walk_V = mkV "kävellä" ;
lin warm_A = mkA "lämmin" "lämpimät" ;
lin water_N = mkN "vesi" ;
lin white_A = mkA "valkoinen" ;
lin wine_N = mkN "viini" ;
lin woman_N = mkN "nainen" ;
lin yellow_A = mkA "keltainen" ;
lin young_A = mkA "nuori" "nuoret";

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- säännölliset substantiivit ja odotettavat muutokset - predictable nouns and expected modifications, e.g. auto - autot, katto - katot, 
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Noun  -- epäsäännölliset substantiivit, irregular nouns like kaupunki - kaupungit, mies - miehet   
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkA = overload {
    mkA : Str -> A
    = \s -> lin A {s = s} ;
    mkA : Str -> Str -> A
    = sg,pl -> lin A (irregA sg pl)
  } ;


  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    --mkV : (inf,pres,part : Str) -> V  -- probably won't need this one with Finnish
    --= \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  --mkV2 = overload {
  --  mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
  --    = \s   -> lin V2 (smartVerb s ** {c = []}) ;
  --  mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
  --    = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
  --  mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
  --    = \v   -> lin V2 (v ** {c = []}) ;
  --  mkV2 : V -> Str -> V2     -- any verb with preposition
  --    = \v,p -> lin V2 (v ** {c = p}) ;
  --  } ;

  --mkAdv : Str -> Adv
  --  = \s -> lin Adv {s = s} ;
  
  --mkPrep : Str -> Prep
  --  = \s -> lin Prep {s = s} ;

}
