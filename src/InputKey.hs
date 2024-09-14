{-# LANGUAGE DeriveGeneric #-}

module InputKey (InputKey (..), keycodeToIK) where

import GHC.Generics (Generic)
import SDL (Keycode (Keycode))

data InputKey
  = KeyUNKNOWN
  | KeyRETURN
  | KeyESCAPE
  | KeyBACKSPACE
  | KeyTAB
  | KeySPACE
  | KeyEXCLAIM
  | KeyQUOTEDBL
  | KeyHASH
  | KeyPERCENT
  | KeyDOLLAR
  | KeyAMPERSAND
  | KeyQUOTE
  | KeyLEFTPAREN
  | KeyRIGHTPAREN
  | KeyASTERISK
  | KeyPLUS
  | KeyCOMMA
  | KeyMINUS
  | KeyPERIOD
  | KeySLASH
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyCOLON
  | KeySEMICOLON
  | KeyLESS
  | KeyEQUALS
  | KeyGREATER
  | KeyQUESTION
  | KeyAT
  | KeyLEFTBRACKET
  | KeyBACKSLASH
  | KeyRIGHTBRACKET
  | KeyCARET
  | KeyUNDERSCORE
  | KeyBACKQUOTE
  | Keya
  | Keyb
  | Keyc
  | Keyd
  | Keye
  | Keyf
  | Keyg
  | Keyh
  | Keyi
  | Keyj
  | Keyk
  | Keyl
  | Keym
  | Keyn
  | Keyo
  | Keyp
  | Keyq
  | Keyr
  | Keys
  | Keyt
  | Keyu
  | Keyv
  | Keyw
  | Keyx
  | Keyy
  | Keyz
  | KeyCAPSLOCK
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyPRINTSCREEN
  | KeySCROLLLOCK
  | KeyPAUSE
  | KeyINSERT
  | KeyHOME
  | KeyPAGEUP
  | KeyDELETE
  | KeyEND
  | KeyPAGEDOWN
  | KeyRIGHT
  | KeyLEFT
  | KeyDOWN
  | KeyUP
  | KeyNUMLOCKCLEAR
  | KeyKP_DIVIDE
  | KeyKP_MULTIPLY
  | KeyKP_MINUS
  | KeyKP_PLUS
  | KeyKP_ENTER
  | KeyKP_1
  | KeyKP_2
  | KeyKP_3
  | KeyKP_4
  | KeyKP_5
  | KeyKP_6
  | KeyKP_7
  | KeyKP_8
  | KeyKP_9
  | KeyKP_0
  | KeyKP_PERIOD
  | KeyAPPLICATION
  | KeyPOWER
  | KeyKP_EQUALS
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyEXECUTE
  | KeyHELP
  | KeyMENU
  | KeySELECT
  | KeySTOP
  | KeyAGAIN
  | KeyUNDO
  | KeyCUT
  | KeyCOPY
  | KeyPASTE
  | KeyFIND
  | KeyMUTE
  | KeyVOLUMEUP
  | KeyVOLUMEDOWN
  | KeyKP_COMMA
  | KeyKP_EQUALSAS400
  | KeyALTERASE
  | KeySYSREQ
  | KeyCANCEL
  | KeyCLEAR
  | KeyPRIOR
  | KeyRETURN2
  | KeySEPARATOR
  | KeyOUT
  | KeyOPER
  | KeyCLEARAGAIN
  | KeyCRSEL
  | KeyEXSEL
  | KeyKP_00
  | KeyKP_000
  | KeyTHOUSANDSSEPARATOR
  | KeyDECIMALSEPARATOR
  | KeyCURRENCYUNIT
  | KeyCURRENCYSUBUNIT
  | KeyKP_LEFTPAREN
  | KeyKP_RIGHTPAREN
  | KeyKP_LEFTBRACE
  | KeyKP_RIGHTBRACE
  | KeyKP_TAB
  | KeyKP_BACKSPACE
  | KeyKP_A
  | KeyKP_B
  | KeyKP_C
  | KeyKP_D
  | KeyKP_E
  | KeyKP_F
  | KeyKP_XOR
  | KeyKP_POWER
  | KeyKP_PERCENT
  | KeyKP_LESS
  | KeyKP_GREATER
  | KeyKP_AMPERSAND
  | KeyKP_DBLAMPERSAND
  | KeyKP_VERTICALBAR
  | KeyKP_DBLVERTICALBAR
  | KeyKP_COLON
  | KeyKP_HASH
  | KeyKP_SPACE
  | KeyKP_AT
  | KeyKP_EXCLAM
  | KeyKP_MEMSTORE
  | KeyKP_MEMRECALL
  | KeyKP_MEMCLEAR
  | KeyKP_MEMADD
  | KeyKP_MEMSUBTRACT
  | KeyKP_MEMMULTIPLY
  | KeyKP_MEMDIVIDE
  | KeyKP_PLUSMINUS
  | KeyKP_CLEAR
  | KeyKP_CLEARENTRY
  | KeyKP_BINARY
  | KeyKP_OCTAL
  | KeyKP_DECIMAL
  | KeyKP_HEXADECIMAL
  | KeyLCTRL
  | KeyLSHIFT
  | KeyLALT
  | KeyLGUI
  | KeyRCTRL
  | KeyRSHIFT
  | KeyRALT
  | KeyRGUI
  | KeyMODE
  | KeyAUDIONEXT
  | KeyAUDIOPREV
  | KeyAUDIOSTOP
  | KeyAUDIOPLAY
  | KeyAUDIOMUTE
  | KeyMEDIASELECT
  | KeyWWW
  | KeyMAIL
  | KeyCALCULATOR
  | KeyCOMPUTER
  | KeyAC_SEARCH
  | KeyAC_HOME
  | KeyAC_BACK
  | KeyAC_FORWARD
  | KeyAC_STOP
  | KeyAC_REFRESH
  | KeyAC_BOOKMARKS
  | KeyBRIGHTNESSDOWN
  | KeyBRIGHTNESSUP
  | KeyDISPLAYSWITCH
  | KeyKBDILLUMTOGGLE
  | KeyKBDILLUMDOWN
  | KeyKBDILLUMUP
  | KeyEJECT
  | KeySLEEP
  deriving (Generic, Eq)

keycodeToIK :: Keycode -> InputKey
keycodeToIK (Keycode 0) = KeyUNKNOWN
keycodeToIK (Keycode 13) = KeyRETURN
keycodeToIK (Keycode 27) = KeyESCAPE
keycodeToIK (Keycode 8) = KeyBACKSPACE
keycodeToIK (Keycode 9) = KeyTAB
keycodeToIK (Keycode 32) = KeySPACE
keycodeToIK (Keycode 33) = KeyEXCLAIM
keycodeToIK (Keycode 34) = KeyQUOTEDBL
keycodeToIK (Keycode 35) = KeyHASH
keycodeToIK (Keycode 37) = KeyPERCENT
keycodeToIK (Keycode 36) = KeyDOLLAR
keycodeToIK (Keycode 38) = KeyAMPERSAND
keycodeToIK (Keycode 39) = KeyQUOTE
keycodeToIK (Keycode 40) = KeyLEFTPAREN
keycodeToIK (Keycode 41) = KeyRIGHTPAREN
keycodeToIK (Keycode 42) = KeyASTERISK
keycodeToIK (Keycode 43) = KeyPLUS
keycodeToIK (Keycode 44) = KeyCOMMA
keycodeToIK (Keycode 45) = KeyMINUS
keycodeToIK (Keycode 46) = KeyPERIOD
keycodeToIK (Keycode 47) = KeySLASH
keycodeToIK (Keycode 48) = Key0
keycodeToIK (Keycode 49) = Key1
keycodeToIK (Keycode 50) = Key2
keycodeToIK (Keycode 51) = Key3
keycodeToIK (Keycode 52) = Key4
keycodeToIK (Keycode 53) = Key5
keycodeToIK (Keycode 54) = Key6
keycodeToIK (Keycode 55) = Key7
keycodeToIK (Keycode 56) = Key8
keycodeToIK (Keycode 57) = Key9
keycodeToIK (Keycode 58) = KeyCOLON
keycodeToIK (Keycode 59) = KeySEMICOLON
keycodeToIK (Keycode 60) = KeyLESS
keycodeToIK (Keycode 61) = KeyEQUALS
keycodeToIK (Keycode 62) = KeyGREATER
keycodeToIK (Keycode 63) = KeyQUESTION
keycodeToIK (Keycode 64) = KeyAT
keycodeToIK (Keycode 91) = KeyLEFTBRACKET
keycodeToIK (Keycode 92) = KeyBACKSLASH
keycodeToIK (Keycode 93) = KeyRIGHTBRACKET
keycodeToIK (Keycode 94) = KeyCARET
keycodeToIK (Keycode 95) = KeyUNDERSCORE
keycodeToIK (Keycode 96) = KeyBACKQUOTE
keycodeToIK (Keycode 97) = Keya
keycodeToIK (Keycode 98) = Keyb
keycodeToIK (Keycode 99) = Keyc
keycodeToIK (Keycode 100) = Keyd
keycodeToIK (Keycode 101) = Keye
keycodeToIK (Keycode 102) = Keyf
keycodeToIK (Keycode 103) = Keyg
keycodeToIK (Keycode 104) = Keyh
keycodeToIK (Keycode 105) = Keyi
keycodeToIK (Keycode 106) = Keyj
keycodeToIK (Keycode 107) = Keyk
keycodeToIK (Keycode 108) = Keyl
keycodeToIK (Keycode 109) = Keym
keycodeToIK (Keycode 110) = Keyn
keycodeToIK (Keycode 111) = Keyo
keycodeToIK (Keycode 112) = Keyp
keycodeToIK (Keycode 113) = Keyq
keycodeToIK (Keycode 114) = Keyr
keycodeToIK (Keycode 115) = Keys
keycodeToIK (Keycode 116) = Keyt
keycodeToIK (Keycode 117) = Keyu
keycodeToIK (Keycode 118) = Keyv
keycodeToIK (Keycode 119) = Keyw
keycodeToIK (Keycode 120) = Keyx
keycodeToIK (Keycode 121) = Keyy
keycodeToIK (Keycode 122) = Keyz
keycodeToIK (Keycode 1073741881) = KeyCAPSLOCK
keycodeToIK (Keycode 1073741882) = KeyF1
keycodeToIK (Keycode 1073741883) = KeyF2
keycodeToIK (Keycode 1073741884) = KeyF3
keycodeToIK (Keycode 1073741885) = KeyF4
keycodeToIK (Keycode 1073741886) = KeyF5
keycodeToIK (Keycode 1073741887) = KeyF6
keycodeToIK (Keycode 1073741888) = KeyF7
keycodeToIK (Keycode 1073741889) = KeyF8
keycodeToIK (Keycode 1073741890) = KeyF9
keycodeToIK (Keycode 1073741891) = KeyF10
keycodeToIK (Keycode 1073741892) = KeyF11
keycodeToIK (Keycode 1073741893) = KeyF12
keycodeToIK (Keycode 1073741894) = KeyPRINTSCREEN
keycodeToIK (Keycode 1073741895) = KeySCROLLLOCK
keycodeToIK (Keycode 1073741896) = KeyPAUSE
keycodeToIK (Keycode 1073741897) = KeyINSERT
keycodeToIK (Keycode 1073741898) = KeyHOME
keycodeToIK (Keycode 1073741899) = KeyPAGEUP
keycodeToIK (Keycode 127) = KeyDELETE
keycodeToIK (Keycode 1073741901) = KeyEND
keycodeToIK (Keycode 1073741902) = KeyPAGEDOWN
keycodeToIK (Keycode 1073741903) = KeyRIGHT
keycodeToIK (Keycode 1073741904) = KeyLEFT
keycodeToIK (Keycode 1073741905) = KeyDOWN
keycodeToIK (Keycode 1073741906) = KeyUP
keycodeToIK (Keycode 1073741907) = KeyNUMLOCKCLEAR
keycodeToIK (Keycode 1073741908) = KeyKP_DIVIDE
keycodeToIK (Keycode 1073741909) = KeyKP_MULTIPLY
keycodeToIK (Keycode 1073741910) = KeyKP_MINUS
keycodeToIK (Keycode 1073741911) = KeyKP_PLUS
keycodeToIK (Keycode 1073741912) = KeyKP_ENTER
keycodeToIK (Keycode 1073741913) = KeyKP_1
keycodeToIK (Keycode 1073741914) = KeyKP_2
keycodeToIK (Keycode 1073741915) = KeyKP_3
keycodeToIK (Keycode 1073741916) = KeyKP_4
keycodeToIK (Keycode 1073741917) = KeyKP_5
keycodeToIK (Keycode 1073741918) = KeyKP_6
keycodeToIK (Keycode 1073741919) = KeyKP_7
keycodeToIK (Keycode 1073741920) = KeyKP_8
keycodeToIK (Keycode 1073741921) = KeyKP_9
keycodeToIK (Keycode 1073741922) = KeyKP_0
keycodeToIK (Keycode 1073741923) = KeyKP_PERIOD
keycodeToIK (Keycode 1073741925) = KeyAPPLICATION
keycodeToIK (Keycode 1073741926) = KeyPOWER
keycodeToIK (Keycode 1073741927) = KeyKP_EQUALS
keycodeToIK (Keycode 1073741928) = KeyF13
keycodeToIK (Keycode 1073741929) = KeyF14
keycodeToIK (Keycode 1073741930) = KeyF15
keycodeToIK (Keycode 1073741931) = KeyF16
keycodeToIK (Keycode 1073741932) = KeyF17
keycodeToIK (Keycode 1073741933) = KeyF18
keycodeToIK (Keycode 1073741934) = KeyF19
keycodeToIK (Keycode 1073741935) = KeyF20
keycodeToIK (Keycode 1073741936) = KeyF21
keycodeToIK (Keycode 1073741937) = KeyF22
keycodeToIK (Keycode 1073741938) = KeyF23
keycodeToIK (Keycode 1073741939) = KeyF24
keycodeToIK (Keycode 1073741940) = KeyEXECUTE
keycodeToIK (Keycode 1073741941) = KeyHELP
keycodeToIK (Keycode 1073741942) = KeyMENU
keycodeToIK (Keycode 1073741943) = KeySELECT
keycodeToIK (Keycode 1073741944) = KeySTOP
keycodeToIK (Keycode 1073741945) = KeyAGAIN
keycodeToIK (Keycode 1073741946) = KeyUNDO
keycodeToIK (Keycode 1073741947) = KeyCUT
keycodeToIK (Keycode 1073741948) = KeyCOPY
keycodeToIK (Keycode 1073741949) = KeyPASTE
keycodeToIK (Keycode 1073741950) = KeyFIND
keycodeToIK (Keycode 1073741951) = KeyMUTE
keycodeToIK (Keycode 1073741952) = KeyVOLUMEUP
keycodeToIK (Keycode 1073741953) = KeyVOLUMEDOWN
keycodeToIK (Keycode 1073741957) = KeyKP_COMMA
keycodeToIK (Keycode 1073741958) = KeyKP_EQUALSAS400
keycodeToIK (Keycode 1073741977) = KeyALTERASE
keycodeToIK (Keycode 1073741978) = KeySYSREQ
keycodeToIK (Keycode 1073741979) = KeyCANCEL
keycodeToIK (Keycode 1073741980) = KeyCLEAR
keycodeToIK (Keycode 1073741981) = KeyPRIOR
keycodeToIK (Keycode 1073741982) = KeyRETURN2
keycodeToIK (Keycode 1073741983) = KeySEPARATOR
keycodeToIK (Keycode 1073741984) = KeyOUT
keycodeToIK (Keycode 1073741985) = KeyOPER
keycodeToIK (Keycode 1073741986) = KeyCLEARAGAIN
keycodeToIK (Keycode 1073741987) = KeyCRSEL
keycodeToIK (Keycode 1073741988) = KeyEXSEL
keycodeToIK (Keycode 1073742000) = KeyKP_00
keycodeToIK (Keycode 1073742001) = KeyKP_000
keycodeToIK (Keycode 1073742002) = KeyTHOUSANDSSEPARATOR
keycodeToIK (Keycode 1073742003) = KeyDECIMALSEPARATOR
keycodeToIK (Keycode 1073742004) = KeyCURRENCYUNIT
keycodeToIK (Keycode 1073742005) = KeyCURRENCYSUBUNIT
keycodeToIK (Keycode 1073742006) = KeyKP_LEFTPAREN
keycodeToIK (Keycode 1073742007) = KeyKP_RIGHTPAREN
keycodeToIK (Keycode 1073742008) = KeyKP_LEFTBRACE
keycodeToIK (Keycode 1073742009) = KeyKP_RIGHTBRACE
keycodeToIK (Keycode 1073742010) = KeyKP_TAB
keycodeToIK (Keycode 1073742011) = KeyKP_BACKSPACE
keycodeToIK (Keycode 1073742012) = KeyKP_A
keycodeToIK (Keycode 1073742013) = KeyKP_B
keycodeToIK (Keycode 1073742014) = KeyKP_C
keycodeToIK (Keycode 1073742015) = KeyKP_D
keycodeToIK (Keycode 1073742016) = KeyKP_E
keycodeToIK (Keycode 1073742017) = KeyKP_F
keycodeToIK (Keycode 1073742018) = KeyKP_XOR
keycodeToIK (Keycode 1073742019) = KeyKP_POWER
keycodeToIK (Keycode 1073742020) = KeyKP_PERCENT
keycodeToIK (Keycode 1073742021) = KeyKP_LESS
keycodeToIK (Keycode 1073742022) = KeyKP_GREATER
keycodeToIK (Keycode 1073742023) = KeyKP_AMPERSAND
keycodeToIK (Keycode 1073742024) = KeyKP_DBLAMPERSAND
keycodeToIK (Keycode 1073742025) = KeyKP_VERTICALBAR
keycodeToIK (Keycode 1073742026) = KeyKP_DBLVERTICALBAR
keycodeToIK (Keycode 1073742027) = KeyKP_COLON
keycodeToIK (Keycode 1073742028) = KeyKP_HASH
keycodeToIK (Keycode 1073742029) = KeyKP_SPACE
keycodeToIK (Keycode 1073742030) = KeyKP_AT
keycodeToIK (Keycode 1073742031) = KeyKP_EXCLAM
keycodeToIK (Keycode 1073742032) = KeyKP_MEMSTORE
keycodeToIK (Keycode 1073742033) = KeyKP_MEMRECALL
keycodeToIK (Keycode 1073742034) = KeyKP_MEMCLEAR
keycodeToIK (Keycode 1073742035) = KeyKP_MEMADD
keycodeToIK (Keycode 1073742036) = KeyKP_MEMSUBTRACT
keycodeToIK (Keycode 1073742037) = KeyKP_MEMMULTIPLY
keycodeToIK (Keycode 1073742038) = KeyKP_MEMDIVIDE
keycodeToIK (Keycode 1073742039) = KeyKP_PLUSMINUS
keycodeToIK (Keycode 1073742040) = KeyKP_CLEAR
keycodeToIK (Keycode 1073742041) = KeyKP_CLEARENTRY
keycodeToIK (Keycode 1073742042) = KeyKP_BINARY
keycodeToIK (Keycode 1073742043) = KeyKP_OCTAL
keycodeToIK (Keycode 1073742044) = KeyKP_DECIMAL
keycodeToIK (Keycode 1073742045) = KeyKP_HEXADECIMAL
keycodeToIK (Keycode 1073742048) = KeyLCTRL
keycodeToIK (Keycode 1073742049) = KeyLSHIFT
keycodeToIK (Keycode 1073742050) = KeyLALT
keycodeToIK (Keycode 1073742051) = KeyLGUI
keycodeToIK (Keycode 1073742052) = KeyRCTRL
keycodeToIK (Keycode 1073742053) = KeyRSHIFT
keycodeToIK (Keycode 1073742054) = KeyRALT
keycodeToIK (Keycode 1073742055) = KeyRGUI
keycodeToIK (Keycode 1073742081) = KeyMODE
keycodeToIK (Keycode 1073742082) = KeyAUDIONEXT
keycodeToIK (Keycode 1073742083) = KeyAUDIOPREV
keycodeToIK (Keycode 1073742084) = KeyAUDIOSTOP
keycodeToIK (Keycode 1073742085) = KeyAUDIOPLAY
keycodeToIK (Keycode 1073742086) = KeyAUDIOMUTE
keycodeToIK (Keycode 1073742087) = KeyMEDIASELECT
keycodeToIK (Keycode 1073742088) = KeyWWW
keycodeToIK (Keycode 1073742089) = KeyMAIL
keycodeToIK (Keycode 1073742090) = KeyCALCULATOR
keycodeToIK (Keycode 1073742091) = KeyCOMPUTER
keycodeToIK (Keycode 1073742092) = KeyAC_SEARCH
keycodeToIK (Keycode 1073742093) = KeyAC_HOME
keycodeToIK (Keycode 1073742094) = KeyAC_BACK
keycodeToIK (Keycode 1073742095) = KeyAC_FORWARD
keycodeToIK (Keycode 1073742096) = KeyAC_STOP
keycodeToIK (Keycode 1073742097) = KeyAC_REFRESH
keycodeToIK (Keycode 1073742098) = KeyAC_BOOKMARKS
keycodeToIK (Keycode 1073742099) = KeyBRIGHTNESSDOWN
keycodeToIK (Keycode 1073742100) = KeyBRIGHTNESSUP
keycodeToIK (Keycode 1073742101) = KeyDISPLAYSWITCH
keycodeToIK (Keycode 1073742102) = KeyKBDILLUMTOGGLE
keycodeToIK (Keycode 1073742103) = KeyKBDILLUMDOWN
keycodeToIK (Keycode 1073742104) = KeyKBDILLUMUP
keycodeToIK (Keycode 1073742105) = KeyEJECT
keycodeToIK (Keycode 1073742106) = KeySLEEP
