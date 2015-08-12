module LibTIB.ItemClasses where

import Data.Word (Word8)

data WeaponClass
    = Mutilator  -- = int32(0x00000046)
    | Starshatter  -- = int32(0x00000047)
    | Striker  -- = int32(0x00000048)
    | Exterminator  -- = int32(0x00000049)
    | Voidblaster  -- = int32(0x0000004a)
    | Ravager  -- = int32(0x0000004b)
    | Brutalizer  -- = int32(0x0000004c)
    | Vaporizer  -- = int32(0x0000004d)
    | Desolator  -- = int32(0x0000004e)
    | Atomizer  -- = int32(0x0000004f)
    | Corruptor  -- = int32(0x00000050)
    | Mindslayer  -- = int32(0x00000051)
    | Riftbreaker  -- = int32(0x00000052)
    | Soultaker  -- = int32(0x00000053)
    | Nullcannon  -- = int32(0x00000054)
    | Demolisher  -- = int32(0x00000055)
    | Incinerator  -- = int32(0x00000056)
    | Eradicator  -- = int32(0x00000057)
    | Rapture  -- = int32(0x0000001e)
    | Glory  -- = int32(0x0000001f)
    | Oblivion  -- = int32(0x00000020)
    | Horror  -- = int32(0x00000021)
    | Ruin  -- = int32(0x00000022)
    | Cataclysm  -- = int32(0x00000023)
    | Torment  -- = int32(0x00000024)
    | Smolder  -- = int32(0x00000025)
    | Destruction  -- = int32(0x00000026)
    | Frenzy  -- = int32(0x00000027)
    | Silence  -- = int32(0x00000028)
    | Exodus  -- = int32(0x00000029)
    | Darkness  -- = int32(0x0000002a)
    | Agony  -- = int32(0x0000002b)
    | Prophecy  -- = int32(0x0000002c)
    | Radiance  -- = int32(0x0000002d)
    | Animus  -- = int32(0x0000002e)
    | Pain  -- = int32(0x0000002f)
    | Screamer  -- = int32(0x00000005)
    | Succubus  -- = int32(0x00000033)
    | Banshee  -- = int32(0x00000034)
    | Basilisk  -- = int32(0x00000035)
    | Harpy  -- = int32(0x00000036)
    | Wyvern  -- = int32(0x00000037)
    | Viper  -- = int32(0x00000038)
    | Ripper  -- = int32(0x00000004)
    | Penatrator  -- = int32(0x0000003a)
    | Serpent  -- = int32(0x0000003b)
    | Hydra  -- = int32(0x0000003c)
    | Firecat  -- = int32(0x0000003d)
    | Hellcannon  -- = int32(0x00000006)
    | Ophidian  -- = int32(0x0000003f)
    | Behemoth  -- = int32(0x00000040)
    | Gargoyle  -- = int32(0x00000041)
    | Kraken  -- = int32(0x00000042)
    | Dragon  -- = int32(0x00000043)
    | BurstCannon  -- = int32(0x0000000a)
    | ProtonLauncher  -- = int32(0x0000000b)
    | AutoCannon  -- = int32(0x00000000)
    | FusionBeam  -- = int32(0x0000000d)
    | Phaser  -- = int32(0x0000000e)
    | MassDriver  -- = int32(0x00000001)
    | GaussCannon  -- = int32(0x00000010)
    | MesonBlaster  -- = int32(0x00000011)
    | OmegaRifle  -- = int32(0x00000012)
    | Leviathan  -- = int32(0x00000002)
    | Accelerator  -- = int32(0x00000014)
    | RailGun  -- = int32(0x00000015)
    | Disruptor  -- = int32(0x00000016)
    | GravitySmasher  -- = int32(0x00000017)
    | Pulverizer  -- = int32(0x00000003)
    | IonCannon  -- = int32(0x00000019)
    | PlasmaLance  -- = int32(0x0000001a)
    | MatterInverter  -- = int32(0x0000001b)
    | WeapIDK Word8
    deriving Show

data ArmorClass
    = TitaniumHull  -- = int32(0x00000000)
    | CompositeHull  -- = int32(0x00000001)
    | CarbideHull  -- = int32(0x00000002)
    | ExopreneShield  -- = int32(0x0000000a)
    | CytoplastShield  -- = int32(0x0000000b)
    | HolocrineShield  -- = int32(0x0000000c)
    | DiamondArmor  -- = int32(0x00000014)
    | ThoriumArmor  -- = int32(0x00000015)
    | OsmiumArmor  -- = int32(0x00000016)
    | CitadelGambit  -- = int32(0x0000001e)
    | AjaxGambit  -- = int32(0x0000001f)
    | AegisGambit  -- = int32(0x00000020)
    | KismetGambit  -- = int32(0x00000021)
    | ArmorIDK Word8
    deriving Show

weap_class c = case c of
    0x00 -> AutoCannon
    0x01 -> MassDriver
    0x02 -> Leviathan
    0x03 -> Pulverizer
    0x04 -> Ripper
    0x05 -> Screamer
    0x06 -> Hellcannon
    0x0a -> BurstCannon
    0x0b -> ProtonLauncher
    0x0d -> FusionBeam
    0x0e -> Phaser
    0x10 -> GaussCannon
    0x11 -> MesonBlaster
    0x12 -> OmegaRifle
    0x14 -> Accelerator
    0x15 -> RailGun
    0x16 -> Disruptor
    0x17 -> GravitySmasher
    0x19 -> IonCannon
    0x1a -> PlasmaLance
    0x1b -> MatterInverter
    0x1e -> Rapture
    0x1f -> Glory
    0x20 -> Oblivion
    0x21 -> Horror
    0x22 -> Ruin
    0x23 -> Cataclysm
    0x24 -> Torment
    0x25 -> Smolder
    0x26 -> Destruction
    0x27 -> Frenzy
    0x28 -> Silence
    0x29 -> Exodus
    0x2a -> Darkness
    0x2b -> Agony
    0x2c -> Prophecy
    0x2d -> Radiance
    0x2e -> Animus
    0x2f -> Pain
    0x33 -> Succubus
    0x34 -> Banshee
    0x35 -> Basilisk
    0x36 -> Harpy
    0x37 -> Wyvern
    0x38 -> Viper
    0x3a -> Penatrator
    0x3b -> Serpent
    0x3c -> Hydra
    0x3d -> Firecat
    0x3f -> Ophidian
    0x40 -> Behemoth
    0x41 -> Gargoyle
    0x42 -> Kraken
    0x43 -> Dragon
    0x46 -> Mutilator
    0x47 -> Starshatter
    0x48 -> Striker
    0x49 -> Exterminator
    0x4a -> Voidblaster
    0x4b -> Ravager
    0x4c -> Brutalizer
    0x4d -> Vaporizer
    0x4e -> Desolator
    0x4f -> Atomizer
    0x50 -> Corruptor
    0x51 -> Mindslayer
    0x52 -> Riftbreaker
    0x53 -> Soultaker
    0x54 -> Nullcannon
    0x55 -> Demolisher
    0x56 -> Incinerator
    0x57 -> Eradicator
    n -> WeapIDK n

armor_class c = case c of
    0x00 -> TitaniumHull
    0x01 -> CompositeHull
    0x02 -> CarbideHull
    0x0a -> ExopreneShield
    0x0b -> CytoplastShield
    0x0c -> HolocrineShield
    0x14 -> DiamondArmor
    0x15 -> ThoriumArmor
    0x16 -> OsmiumArmor
    0x1e -> CitadelGambit
    0x1f -> AjaxGambit
    0x20 -> AegisGambit
    0x21 -> KismetGambit
    n -> ArmorIDK n
