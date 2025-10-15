*&---------------------------------------------------------------------*
*&  Include           ZXF01U01
*&---------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(I_FEBEP) LIKE  FEBEP STRUCTURE  FEBEP
*"     VALUE(I_FEBKO) LIKE  FEBKO STRUCTURE  FEBKO
*"     VALUE(I_TESTRUN) TYPE  XFLAG
*"  EXPORTING
*"     VALUE(E_FEBEP) LIKE  FEBEP STRUCTURE  FEBEP
*"     VALUE(E_FEBKO) LIKE  FEBKO STRUCTURE  FEBKO
*"     VALUE(E_MSGTEXT) LIKE  FEBMKA-MESSG
*"     VALUE(E_MSGTYP) LIKE  FEBMKA-MSTYP
*"     VALUE(E_UPDATE) LIKE  FEBMKA-MSTYP
*"  TABLES
*"      T_FEBCL STRUCTURE  FEBCL
*"      T_FEBRE STRUCTURE  FEBRE

  E_FEBEP       = I_FEBEP.
  E_FEBEP-GSBER = I_FEBKO-BUKRS.
  E_FEBEP-ZUONR = E_FEBEP-VALUT. " loreni 26.10.2015 ALRS
