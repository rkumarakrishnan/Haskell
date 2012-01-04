module Music where

    import Ratio

    -- Nota
    type Pitch = (PitchClass, Octave)

    -- Classes de nota
    data PitchClass = Cf | C | Cs | -- Dó bemol (existe?), Dó e Dó sustenido
		      Df | D | Ds | -- Ré bemol, Ré e Ré sustenido
		      Ef | E | Es |
		      Ff | F | Fs |
		      Gf | G | Gs |
		      Af | A | As |
		      Bf | B | Bs   -- Si bemol, Si e Si sustenido (existe?)
	 deriving (Eq, Show)

    -- Oitava
    type Octave = Int

    -- Música
    data Music = Note Pitch Dur
	       | Rest Dur
	       | Music :+: Music
	       | Music :=: Music
	       | Tempo (Ratio Int) Music
	       | Trans Int Music
	       | Instr IName Music
	 deriving (Eq, Show)

    -- Duração
    type Dur = Ratio Int

    -- Nota absoluta
    type AbsPitch = Int

    -- Convert uma nota (classe + oitava) em nota absoluta
    absPitch :: Pitch -> AbsPitch
    absPitch (pitch, octave) = 12 * octave + pcToInt pitch

    -- Converte uma nota absoluta para nota (classe + oitava)
    pitch :: AbsPitch -> Pitch
    pitch ap = ([C,Cs,D,D,Ds,E,F,Fs,G,Gs,A,As,B] !! mod ap 12, quot ap 12)

    -- Converte uma classe de nota para inteiro
    pcToInt :: PitchClass -> Int
    pcToInt pc = case pc of
		    Cf -> -1 -- Ou 11?
		    C  -> 0
		    Cs -> 1
		    Df -> 1
		    D  -> 2
		    Ds -> 3
		    Ef -> 3
		    E  -> 4
		    Es -> 5
		    Ff -> 4
		    F  -> 5
		    Fs -> 6
		    Gf -> 6
		    G  -> 7
		    Gs -> 8
		    Af -> 8
		    A  -> 9
		    As -> 10
		    Bf -> 10
		    B  -> 11
		    Bs -> 12 -- Ou 0?

    -- Desloca uma nota por um dado número de oitavas
    trans :: Int -> Pitch -> Pitch
    trans i p = pitch (absPitch p + i)

    -- Define os instrumentos que podem ser usados para executar uma Music, cf. especificado pelo padrão MIDI
    data IName 
	= AcousticGrandPiano  | BrightAcousticPiano | ElectricGrandPiano
	| HonkyTonkPiano      | RhodesPiano         | ChorusedPiano
	| Harpsichord   | Clavinet        | Celesta | Glockenspiel  | MusicBox
	| Vibraphone | Marimba  | Xylophone           | TubularBells
	| Dulcimer              | HammondOrgan        | PercussiveOrgan 
	| RockOrgan | ChurchOrgan         | ReedOrgan
	| Accordion             | Harmonica           | TangoAccordion
	| AcousticGuitarNylon   | AcousticGuitarSteel | ElectricGuitarJazz
	| ElectricGuitarClean   | ElectricGuitarMuted | OverdrivenGuitar
	| DistortionGuitar      | GuitarHarmonics     | AcousticBass
	| ElectricBassFingered  | ElectricBassPicked  | FretlessBass
	| SlapBass1             | SlapBass2           | SynthBass1 | SynthBass2
	| Violin        | Viola | Cello  | Contrabass | TremoloStrings
	| PizzicatoStrings      | OrchestralHarp      | Timpani
	| StringEnsemble1       | StringEnsemble2     | SynthStrings1
	| SynthStrings2         | ChoirAahs           | VoiceOohs | SynthVoice
	| OrchestraHit          | Trumpet             | Trombone  | Tuba 
	| MutedTrumpet          | FrenchHorn          | BrassSection | SynthBrass1
	| SynthBrass2           | SopranoSax          | AltoSax | TenorSax 
	| BaritoneSax    | Oboe | Bassoon  | EnglishHorn          | Clarinet
	| Piccolo               | Flute    | Recorder | PanFlute  | BlownBottle
	| Shakuhachi            | Whistle  | Ocarina  | Lead1Square
	| Lead2Sawtooth         | Lead3Calliope       | Lead4Chiff
	| Lead5Charang          | Lead6Voice          | Lead7Fifths
	| Lead8BassLead         | Pad1NewAge          | Pad2Warm
	| Pad3Polysynth         | Pad4Choir           | Pad5Bowed
	| Pad6Metallic          | Pad7Halo            | Pad8Sweep
	| FX1Train              | FX2Soundtrack       | FX3Crystal
	| FX4Atmosphere         | FX5Brightness       | FX6Goblins
	| FX7Echoes             | FX8SciFi            | Sitar | Banjo  | Shamisen
	| Koto | Kalimba        | Bagpipe             | Fiddle | Shanai
	| TinkleBell    | Agogo | SteelDrums          | Woodblock      | TaikoDrum
	| MelodicDrum           | SynthDrum           | ReverseCymbal
	| GuitarFretNoise       | BreathNoise         | Seashore
	| BirdTweet             | TelephoneRing       | Helicopter
	| Applause              | Gunshot             | Percussion
	deriving (Show,Eq,Ord,Enum)

    -- As funções cf, c, cs... definem a nota (dó bemol, dó, dó sustenido...), e para gerar uma Music, precisam receber uma oitava (Octave) e uma duração (Dur)
    cf, c, cs, df, d, ds, ef, e, es, ff, f, fs, gf, g, gs, af, a, as, bf, b, bs :: Octave -> Dur -> Music
 
    cf o = Note (Cf, o); c o = Note (C, o); cs o = Note (Cs, o)
    df o = Note (Df, o); d o = Note (D, o); ds o = Note (Ds, o)
    ef o = Note (Ef, o); e o = Note (E, o); es o = Note (Es, o)
    ff o = Note (Ff, o); f o = Note (F, o); fs o = Note (Fs, o)
    gf o = Note (Gf, o); g o = Note (G, o); gs o = Note (Gs, o)
    af o = Note (Af, o); a o = Note (A, o); as o = Note (As, o)
    bf o = Note (Bf, o); b o = Note (B, o); bs o = Note (Bs, o)

    -- Algumas durações (Dur) comumente utilizadas
    wn, hn, qn, en, sn, tn :: Dur
    dhn, dqn, den, dsn :: Dur

    -- Algumas pausas (Music, com construtor Rest) comumente utilizadas
    wnr, hnr, qnr, enr, snr, tnr :: Music
    dhnr, dqnr, denr, dsnr :: Music

    wn = 1; wnr = Rest wn -- Duração unitária
    hn = 1%2; hnr = Rest hn -- Meia
    qn = 1%4; qnr = Rest qn -- Quarta
    en = 1%8; enr = Rest en -- Oitava
    sn = 1%16; snr = Rest sn -- Um desesseis avos
    tn = 1%32; tnr = Rest tn -- Um trinta e dois avos

    dhn = 3%4; dhnr = Rest dhn -- dotted half
    dqn = 3%8; dqnr = Rest dqn -- dotted quarter
    den = 3%16; denr = Rest den -- dotted eighth
    dsn = 3%32; dsnr = Rest dsn -- dotted sixteenth

    -- line recebe uma lista de músicas e retorna uma música que é a concatenação das músicas dadas
    -- chord recebe uma lista de músicas e retorna um acorde
    line, chord :: Music -> Music
    line = foldr (:+:) (Rest 0)
    chord = foldr (:=:) (Rest 0)

    -- Atrasa a execução de uma música (Music)
    delay :: Dur -> Music -> Music
    delay d m = Rest d :+: m

    -- Repete indefinidamente uma música
    repeatM :: Music -> Music
    repeatM m = m :+: repeatM m

    -- dur retorna a duração (Dur) de uma música (Music)
    dur :: Music -> Dur
    dur (Note _ d) = d
    dur (Rest d)   = d
    dur (m1 :+: m2) = dur m1 + dur m2
    dur (m1 :=: m2) = dur m1 `max` dur m2
    dur (Tempo a m) = dur m/a
    dur (Trans _ m) = dur m
    dur (Instr _ m) = dur m

    -- Inverte uma música (toca-a de trás para frente)
    revM :: Music -> Music
    revM n@(Note _ _) = n
    revM r@(Rest _ _) = r
    revM (Tempo a m) = Tempo a (revM m)
    revM (Trans i m) = Trans i (revM m)
    revM (Instr i m) = Instr i (revM m)
    revM (m1 :+: m2) = revM m2 :+: revM m1
    revM (m1 :=: m2)
	= let d1 = dur m1
	      d2 = dur m2
	  in if d1 > d2 then revM m1 :=: (Rest (d1 - d2) :+: revM m2)
	                else (Rest (d2 - d1) :+: revM m1) :=: revM m2

    -- A função cut é auxiliar, utilizada pelo operador /=:
    cut :: Dur -> Music -> Music
    cut d m | d <= 0   = Rest 0
    cut d (Note x d0)  = Note x (min d d0)
    cut d (Rest d0)    = Rest (min d d0)
    cut d (m1 :=: m2)  = cut d m1 :=: cut d m2
    cut d (Tempo a m)  = Tempo a (cut (d*a) m)
    cut d (Tans a m)   = Trans a (cut d m)
    cut d (Instr i m)  = Instr i (cut d m)
    cut d (m1 :+: m2)  = let m1' = cut d m1
			     m2' = cut (d - dur m1) m2
			 in m1' :+: m2'

    -- O operador /=: cria um acorde com a menor das duas músicas dadas
    (/=:) :: Music -> Music -> Music
    m1 /=: m2 = cut (min (dur m1) (dur m2)) (m1 :=: m2)
