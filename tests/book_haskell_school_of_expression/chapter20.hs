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

