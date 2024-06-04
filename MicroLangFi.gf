--# -path=.:../abstract
concrete MicroLangFi of MicroLang = open MicroResFi, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat --linearization **type** definitions

    Utt = {s : Str} ;
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Number => Case => Str } ; 
    Comp = {s : Number => Case => Str} ;   
    AP = Adjective ;
    CN = Noun ;    --Noun : Type = {s : Number => Case => Str} ;  
    NP, Pron = {s : Case => Str ; n : Number ; p : Person} ;   
    Det = {s : Str ; n : Number; cs : Case} ;
    Prep = {s : Str} ;
    V = {s : Number => Person => Tense => Str} ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin  --linearization definitions
  
  UttS s = s ;
  UttNP np = {s = np.s ! Nom } ; 

  PredVPS np vp = {  -- WE COULD ALSO USE PRESENT PERFECT OR PAST PERFECT TENSES HERE (INSTEAD OF PRES: PER OR PKP) :)
    s = np.s ! Nom  ++ vp.verb.s ! np.n ! np.p ! Pres ++ vp.compl ! np.n ! Nom 
    } ;

   UseV v = {
      verb = v ;
      compl = \\n,c => "" ;
      } ;
      
  ComplV2 v2 np = {  --love it
    verb = v2 ;
    compl = \\n,c => v2.c  ++ np.s ! Par    -- NP object in the accusative  
    } ;
      
  UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = comp.s      
      } ;
     
  CompAP ap = {s = ap.s};     
      
  AdvVP vp adv =  --sleep here
      vp ** {compl = \\n,c => vp.compl ! n ! c ++ adv.s} ;

--WORKING VERSION:
DetCN det cn = {
    s = \\cs => det.s ++ cn.s ! det.n ! cs ;
    n = det.n;
    p = Per3;
} ;
      
  UsePron p = p;
            
  a_Det = {s = "" ; n = Sg ; cs = Nom} ; ---will always be empty in Finnish since there are no articles per se
  aPl_Det = {s = "" ; n = Pl ; cs = Nom} ;
  the_Det = {s = "" ; n = Sg ; cs = Nom} ;
  thePl_Det = {s = "" ; n = Pl ; cs = Nom} ;
    
  UseN n = n ;
    
  AdjCN ap cn = {
    s = table {
      n => table { 
        c => ap.s ! n ! c ++ cn.s ! n ! c  --number and case so that they are inflected correctly (altho I guess case will always be Nom(?))
          }
       }  
    } ;

    PositA a = a ;

    PrepNP prep np = {s = np.s ! Acc ++ prep.s} ;  

    in_Prep = {s = "sisällä"} ; 
    on_Prep = {s = "päällä"} ; 
    with_Prep = {s = "kanssa"} ;

     me_Pron = {
       s = table {
            Nom => "minä";
            Acc => "minut";
            Gen => "minun";
            Par => "minua";
            Ine => "minussa";
            Ela => "minusta";
            Ill => "minuun";
            Ade => "minulla";
            Abl => "minulta";
            All => "minulle";
            Ess => "minuna";
            Tra => "minuksi";
            Abe => "_" };
       n = Sg ;
       p = Per1
       } ;

      you_Pron = {
       s = table {
            Nom => "sinä";
            Acc => "sinut";
            Gen => "sinun";
            Par => "sinua";
            Ine => "sinussa";
            Ela => "sinusta";
            Ill => "sinuun";
            Ade => "sinulla";
            Abl => "sinulta";
            All => "sinulle";
            Ess => "sinuna";
            Tra => "sinuksi";
            Abe => "_" };
       n = Sg ;
       p = Per2
       } ;

     he_Pron = {
       s = table {
            Nom => "hän";
            Acc => "hänet";
            Gen => "hänen";
            Par => "häntä";
            Ine => "hänessä";
            Ela => "hänestä";
            Ill => "häneen";
            Ade => "hänellä";
            Abl => "häneltä";
            All => "hänelle";
            Ess => "hänenä";
            Tra => "häneksi";
            Abe => "_" };
       n = Sg ;
       p = Per3
       } ;
     she_Pron = {
       s = table {
            Nom => "hän";
            Acc => "hänet";
            Gen => "hänen";
            Par => "häntä";
            Ine => "hänessä";
            Ela => "hänestä";
            Ill => "häneen";
            Ade => "hänellä";
            Abl => "häneltä";
            All => "hänelle";
            Ess => "hänenä";
            Tra => "häneksi";
            Abe => "_" };
       n = Sg;
       p = Per3
       } ;
       we_Pron = {
       s = table {
            Nom => "me";
            Acc => "meidät";
            Gen => "meidän";
            Par => "meitä";
            Ine => "meissä";
            Ela => "meistä";
            Ill => "meihin";
            Ade => "meillä";
            Abl => "meiltä";
            All => "meille";
            Ess => "meinä";
            Tra => "meiksi";
            Abe => "_" };
      n = Pl;
      p = Per1
      } ;
      you2_Pron = {
       s = table {
            Nom => "te";
            Acc => "teidät";
            Gen => "teidän";
            Par => "teitä";
            Ine => "teissä";
            Ela => "teistä";
            Ill => "teihin";
            Ade => "teillä";
            Abl => "teiltä";
            All => "teille";
            Ess => "teinä";
            Tra => "teiksi";
            Abe => "_" };
      n = Pl;
      p = Per2
      } ;
    they_Pron = {
       s = table {
            Nom => "he";
            Acc => "heidät";
            Gen => "heidän";
            Par => "heitä";
            Ine => "heissä";
            Ela => "heistä";
            Ill => "heihin";
            Ade => "heillä";
            Abl => "heiltä";
            All => "heille";
            Ess => "heinä";
            Tra => "heiksi";
            Abe => "_" };
      n = Pl;
      p = Per3
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "jo" ;
lin animal_N = mkN "eläin" ;
lin apple_N = mkN "omena" ; 
lin baby_N = mkN "vauva" ; 
lin bad_A = mkA "paha" ;
lin beer_N = mkN "olut" ;
lin big_A = mkA "iso" ;
lin bike_N = mkN "polkupyörä" ; 
lin bird_N = mkN "lintu" ;
lin black_A = mkA "musta" ;
lin blood_N = mkN "veri" ;
lin blue_A = mkA "sininen" ;
lin boat_N = mkN "vene" ;
lin book_N = mkN "kirja" ;
lin boy_N = mkN "poika" ;
lin bread_N = mkN "leipä" ;
lin break_V2 = mkV2 "rikkoa" ; 
lin buy_V2 = mkV2 "ostaa" ;
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
lin drink_V2 = mkV2 "juoda" ; 
lin eat_V2 = mkV2 "syödä" ;   
lin find_V2 = mkV2 "löytää" "löydä" "löytänyt" "löytäneet" ; --the order of forms given INF, STEM, PAST PARTICIPLE SG, PAST PARTICIPLE PL
lin fire_N = mkN "tuli" ;
lin fish_N = mkN "kala" ;
lin flower_N = mkN "kukka" ;
lin friend_N = mkN "ystävä" ;
lin girl_N = mkN "tyttö" ;
lin good_A = mkA "hyvä" ;
lin go_V = mkV "mennä" ;      
lin grammar_N = mkN "kielioppi" ;
lin green_A = mkA "vihreä" ;
lin heavy_A = mkA "painava" ;
lin horse_N = mkN "hevonen" ;
lin hot_A = mkA "kuuma" ;
lin house_N = mkN "talo" ;
--lin john_PN = mkPN "John" ;
lin jump_V = mkV "hypätä" ;
lin kill_V2 = mkV2 "tappaa" ;
--lin know_VS = mkVS (mkV "know" "knew" "known") ;
lin language_N = mkN "kieli" ;
lin live_V = mkV "elää" ;
lin love_V2 = mkV2 "rakastaa" ; 
lin man_N = mkN "mies" ;
lin milk_N = mkN "maito" ;
lin music_N = mkN "musiikki" ;
lin new_A = mkA "uusi" "uudet" ;
lin now_Adv = mkAdv "nyt" ;
lin old_A = mkA "vanha" ;
--lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "pelata" ;
lin read_V2 = mkV2 "lukea" "lue" "lukenut" "lukeneet" ; --the order of forms given INF, STEM, PAST PARTICIPLE SG, PAST PARTICIPLE PL
lin ready_A = mkA "valmis" "valmiit" ;
lin red_A = mkA "punainen" ;
lin river_N = mkN "joki" ;
lin run_V = mkV "juosta" "juokse" "juossut" "juosseet" ; --the order of forms given INF, STEM, PAST PARTICIPLE SG, PAST PARTICIPLE PL
lin sea_N = mkN "meri" ;
lin see_V2 = mkV2 "nähdä" "näe" "nähnyt" "nähneet"; --the order of forms given INF, STEM, PAST PARTICIPLE SG, PAST PARTICIPLE PL
lin ship_N = mkN "laiva" ;
lin sleep_V = mkV "nukkua" ;
lin small_A = mkA "pieni" "pienet" ;
lin star_N = mkN "tähti" ;
lin swim_V = mkV "uida" ;
lin teach_V2 = mkV2  "opettaa" ;     
lin train_N = mkN "juna" ;
lin travel_V = mkV "matkustaa" ;
lin tree_N = mkN "puu" ;
lin understand_V2 = mkV2 "ymmärtää" "ymmärrä" "ymmärtänyt" "ymmärtäneet" ;       
lin wait_V2 = mkV2 "odottaa" ;
lin walk_V = mkV "kävellä" ;
lin warm_A = mkA "lämmin" "lämpimät" ;
lin water_N = mkN "vesi" ;
lin white_A = mkA "valkoinen" ;
lin wine_N = mkN "viini" ;
lin woman_N = mkN "nainen" ;
lin yellow_A = mkA "keltainen" ;
lin young_A = mkA "nuori" "nuoret";

-- own added words (have also been added to abstract grammar MicroLang!)
lin helmet_N = mkN "kypärä" ;
lin pencil_N = mkN "kynä" ;
lin cake_N = mkN "kakku";
lin ticket_N = mkN "lappu" ;
lin cloudberry_N = mkN "lakka";
lin hat_N = mkN "hattu";
lin stairway_N = mkN "rappu";
lin cap_N = mkN "lakki";
lin button_N = mkN "nappi";
lin pacifier_N = mkN "tutti";

---------------------------
-- Paradigms part ---------
---------------------------

oper
  --nouns
  mkN = overload {
    mkN : Str -> Noun   -- säännölliset substantiivit ja odotettavat muutokset - predictable nouns and expected modifications, e.g. auto - autot, katto - katot, 
      = \n -> lin N (smartNoun n) ; --smartNoun
    mkN : Str -> Str -> Noun  -- epäsäännölliset substantiivit, irregular nouns like kaupunki - kaupungit, mies - miehet   
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;
  --adjectives
  mkA = overload {
    mkA : Str -> A
    = \s -> lin A (mkAdjective s) ; --used to be lin A { s = s}
    mkA : Str -> Str -> A
    = \sg,pl -> lin A (irregA sg pl) ; --irregular adjectives, there arent many
  } ;

-- intransitive verbs
  mkV = overload {
    mkV : (inf : Str) -> V  
      = \s -> lin V (smartVerb s) ; --most of our Finnish verbs go through the pattern checking in smartVerb
    mkV : Str -> Str -> Str -> Str -> V  --only needed with a few of the forms
      = \inf, stem, persg, perpl -> lin V (irregVerb inf stem persg perpl) ;
  } ;

 -- transitive verbs
 mkV2 = overload {
  mkV2 : (inf : Str) -> V2           -- predictable verb with direct object, e.g. "opettaa" "juoda"
    = \s   -> lin V2 (smartVerb s ** {c = []}); 
  mkV2 : Str -> Str -> Str -> Str -> V2            -- irregular verb with direct object, e.g. "lukea"
    = \v,y,e,w   -> lin V2 (irregVerb v y e w ** {c = []}) ;
 } ;

  --adverbs
  mkAdv : Str -> Adv
   = \s -> lin Adv {s = s} ;
  
  --prepositions
  mkPrep : Str -> Prep
   = \s -> lin Prep {s = s} ;

}
