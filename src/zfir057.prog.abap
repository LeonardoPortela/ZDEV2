*&---------------------------------------------------------------------*
*& Report  ZFIR056
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFIR057.

TABLES: ZFIT0098, ZFIT0099, ZFIT0100, ZFIT0101, ZFIT0104, BSAD, BSID.

"Variaveis
DATA: VG_IMPRIMIR_REL TYPE C.


"Filtros
DATA: VL_BUDAT_FIM TYPE BSID-BUDAT,
      VL_AUGDT_INI TYPE BSID-AUGDT,
      VL_AUGDT_FIM TYPE BSID-AUGDT,
      VL_DT_ALL_INI TYPE BSID-BUDAT,
      VL_DT_ALL_FIM TYPE BSID-BUDAT.

DATA: WA_FILTRO_TERRENO TYPE ZFIT0099,
      WA_FILTRO_CLIENTE TYPE ZFIT0100.

DATA: R_ST1       TYPE C LENGTH 1,
      R_ST2       TYPE C LENGTH 1,
      R_ST3       TYPE C LENGTH 1 VALUE 'X'.

RANGES: P_BUDAT   FOR BSID-BUDAT       , " Partidas em Aberto
        P_AUGDT   FOR BSAD-AUGDT       , " Partidas Compensadas
        P_DT_ALL  FOR BSID-BUDAT       , " Todas Partidas

        P_EMP     FOR ZFIT0101-EMPRESA,    " Empresa
        P_LOTE    FOR ZFIT0101-LOTEAMENTO  , " Loteamento
        P_QUADRA  FOR ZFIT0101-NRO_QUADRA  , " Quadra
        P_TERR    FOR ZFIT0101-NRO_TERRENO , " Terreno
        P_KUNNR   FOR ZFIT0100-CLIENTE .     " Cliente

*SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS:
*    P_EMP     FOR ZFIT0101-EMPRESA     NO INTERVALS NO-EXTENSION OBLIGATORY, " Empresa
*    P_LOTE    FOR ZFIT0101-LOTEAMENTO  , " Loteamento
*    P_QUADRA  FOR ZFIT0101-NRO_QUADRA  , " Quadra
*    P_TERR    FOR ZFIT0101-NRO_TERRENO , " Terreno
*    P_KUNNR   FOR ZFIT0100-CLIENTE . " Cliente
*SELECTION-SCREEN: END OF BLOCK B1.

*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
*PARAMETERS:
**   R_ST1       RADIOBUTTON GROUP TP,
**   R_ST2       RADIOBUTTON GROUP TP,
**   R_ST3       RADIOBUTTON GROUP TP DEFAULT 'X'.
**SELECT-OPTIONS:
**
**    P_BUDAT   FOR BSID-BUDAT       , " Partidas em Aberto
**    P_AUGDT   FOR BSAD-AUGDT       , " Partidas Compensadas
**    P_DT_ALL  FOR BSID-BUDAT       . " Todas Partidas
*SELECTION-SCREEN END   OF BLOCK B2.

INITIALIZATION.

CALL SCREEN 0100.

INCLUDE ZFIR057_TOP.
INCLUDE ZFIR057_FORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'SEA'.
      PERFORM VALIDA_CAMPOS.

      PERFORM CONFIG_RANGES.

      CHECK VG_IMPRIMIR_REL IS NOT INITIAL.

      PERFORM INICIAR_VARIAVEIS.
      PERFORM SELECIONA_DADOS.
      PERFORM PROCESSA_DADOS.
      PERFORM IMPRIMIR_DADOS.
    WHEN 'PSQ_QD'.
      PERFORM PSQ_QD_LOW.
    WHEN 'PSQ_QD_HIGH'.
      PERFORM PSQ_QD_HIGH.
    WHEN 'RADIO'.
       IF R_ST1 IS NOT INITIAL.
         CLEAR: P_AUGDT, P_DT_ALL.
       ENDIF.

       IF R_ST2 IS NOT INITIAL.
         CLEAR: P_BUDAT, P_DT_ALL.
       ENDIF.

        IF R_ST3 IS NOT INITIAL.
         CLEAR: P_BUDAT ,P_AUGDT.
       ENDIF.


  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
