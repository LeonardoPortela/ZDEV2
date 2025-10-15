*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| MTM FINANCEIRO - RELATÓRIOS                                               |*
*| Notas Explicativas:                                                       |*
*|                    NDF                                                    |*
*|                    SWAP VANILLA                                           |*
*|                    SWAP FLUXO                                             |*
*| Exposição Derivativa.                                                     |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@amaggi.com.br )                |*
*|  Tester:                                                                  |*
*|    + Paulo Quevedo ( paulo.quevedo@grupomaggi.com.br )                    |*
*|                                                                           |*
*/===========================================================================\*
REPORT  ZFIR0057.

"***********************
" Tabelas.
"***********************
TABLES: ZFIT0064.

*----------------------------------------------------------------------*
* TAB CONTROLS
*----------------------------------------------------------------------*
CONTROLS TABSTRIP TYPE TABSTRIP. "Controle do Tabstrip

*----------------------------------------------------------------------*
* OBJETOS DO ALV
*----------------------------------------------------------------------*
DATA: OBJ_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV         TYPE REF TO CL_GUI_ALV_GRID,
      GT_FIELDCATALOG TYPE LVC_T_FCAT,
      GW_FIELDCATALOG TYPE LVC_S_FCAT.

*----------------------------------------------------------------------*
* CONSTANTES
*----------------------------------------------------------------------*
CONSTANTS: TELA_0100 TYPE SY-DYNNR VALUE '0100', "Tela Principal das Tabs
           TELA_0200 TYPE SY-DYNNR VALUE '0200', "Tela Principal para Exposição Derivativo
           TELA_0300 TYPE SY-DYNNR VALUE '0300', "Tela NDF
           TELA_0400 TYPE SY-DYNNR VALUE '0400', "Tela SWAP Vanilla
           TELA_0500 TYPE SY-DYNNR VALUE '0500', "Tela SWAP Flux
           TELA_0600 TYPE SY-DYNNR VALUE '0600'. "Tela Exposição Derivativo.

CONSTANTS: TAB_NDF       TYPE C LENGTH 7  VALUE 'TAB_NDF',       "TAB para NDF
           TAB_SWAP_VAN  TYPE C LENGTH 12 VALUE 'TAB_SWAP_VAN',  "TAB Swap Vanilla
           TAB_SWAP_FLUX TYPE C LENGTH 13 VALUE 'TAB_SWAP_FLUX'. "TAB Swap Fluxo

*----------------------------------------------------------------------*
* Estrutura
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_SAIDA,
          TP_POSICAO     TYPE ZFIT0064-TP_POSICAO,
          MOEDA          TYPE C LENGTH 15,
          DT_FIM_CTO     TYPE C LENGTH 10,
          VLR_OPERACAO   TYPE ZFIT0064-VLR_OPERACAO,
          VLR_PRES_MTM   TYPE ZFIT0064-VLR_PRES_MTM,
          VLR_PRES_MTM_R TYPE ZFIT0064-VLR_PRES_MTM,
          BANCO          TYPE ZFIT0064-BANCO,
          TP_AJUSTE      TYPE ZFIT0064-TP_AJUSTE,
          VLR_AJ_MERC    TYPE ZFIT0069-VLR_AJ_MERC,
          CHECK          TYPE C LENGTH 1,
       END OF TY_SAIDA,

       BEGIN OF TY_ZFIT0064,
          TP_POSICAO    TYPE ZFIT0064-TP_POSICAO,
          MOEDA         TYPE ZFIT0064-MOEDA,
          DT_FIM_CTO    TYPE ZFIT0064-DT_FIM_CTO,
          VLR_OPERACAO  TYPE ZFIT0064-VLR_OPERACAO,
          VLR_PRES_MTM  TYPE ZFIT0064-VLR_PRES_MTM,
          TP_AJUSTE     TYPE ZFIT0064-TP_AJUSTE,
          BANCO         TYPE ZFIT0064-BANCO,
         DT_FIM_CTO_AUX TYPE C LENGTH 4,
       END OF TY_ZFIT0064,

       BEGIN OF TY_ZFIT0069,
        DT_FIM_CTO       TYPE ZFIT0069-DT_FIM_CTO,
        VLR_OPERACAO     TYPE ZFIT0069-VLR_OPERACAO,
        VLR_MTM_P_ATIV   TYPE ZFIT0069-VLR_MTM_P_ATIV,
        VLR_MTM_P_PASSIV TYPE ZFIT0069-VLR_MTM_P_PASSIV,
        VLR_OPERACAO_INT TYPE ZFIT0069-VLR_OPERACAO_INT,
        TP_AJUSTE        TYPE ZFIT0069-TP_AJUSTE,
        BANCO            TYPE ZFIT0069-BANCO,
        VLR_AJ_MERC      TYPE ZFIT0069-VLR_AJ_MERC,
        DT_FIM_CTO_AUX   TYPE C LENGTH 4,
       END OF TY_ZFIT0069,


       BEGIN OF TY_EXPOSICAO_DERIVATIVO,
            TIPO         TYPE C LENGTH 10,
            TP_AJUSTE    TYPE ZFIT0064-TP_AJUSTE,
            VALOR        TYPE ZFIT0064-VLR_PRES_MTM,
            BANCO        TYPE ZFIT0064-BANCO,
       END OF TY_EXPOSICAO_DERIVATIVO,

       BEGIN OF TY_SAIDA_EXPOSICAO_DERIVATIVO,
            BANCO             TYPE ZFIT0064-BANCO,
            VLR_ATIVO_NDF     TYPE ZFIT0064-VLR_PRES_MTM,
            VLR_PASSIVO_NDF   TYPE ZFIT0064-VLR_PRES_MTM,
            VLR_ATIVO_SWAPV   TYPE ZFIT0064-VLR_PRES_MTM,
            VLR_PASSIVO_SWAPV TYPE ZFIT0064-VLR_PRES_MTM,
            VLR_ATIVO_SWAPF   TYPE ZFIT0064-VLR_PRES_MTM,
            VLR_PASSIVO_SWAPF TYPE ZFIT0064-VLR_PRES_MTM,

            TOTAL_ATIVO       TYPE ZFIT0064-VLR_PRES_MTM,
            TOTAL_PASSIVO     TYPE ZFIT0064-VLR_PRES_MTM,
            SALDO             TYPE ZFIT0064-VLR_PRES_MTM,

       END OF TY_SAIDA_EXPOSICAO_DERIVATIVO.

*----------------------------------------------------------------------*
* Internal Table
*----------------------------------------------------------------------*
DATA: GT_ZFIT0064             TYPE TABLE OF TY_ZFIT0064,
      GT_ZFIT0069             TYPE TABLE OF TY_ZFIT0069,
      GT_ZFIT0069_ATIVO       TYPE TABLE OF TY_ZFIT0069,
      GT_ZFIT0069_PASSIVO     TYPE TABLE OF TY_ZFIT0069,
      GT_ZFIT0070             TYPE TABLE OF ZFIT0070,
      GT_ZFIT0070_ATIVO       TYPE TABLE OF ZFIT0070,
      GT_ZFIT0070_PASSIVO     TYPE TABLE OF ZFIT0070,
      GT_ZFIT0067             TYPE TABLE OF ZFIT0067,
      GT_ZFIT0067_ATIVO       TYPE TABLE OF ZFIT0067,
      GT_ZFIT0067_PASSIVO     TYPE TABLE OF ZFIT0067,
      GT_EXPOSICAO_DERIVATIVO TYPE TABLE OF TY_EXPOSICAO_DERIVATIVO,
      GT_SAIDA_DERIVATIVO     TYPE TABLE OF TY_SAIDA_EXPOSICAO_DERIVATIVO,
      GT_SAIDA                TYPE TABLE OF TY_SAIDA.

*----------------------------------------------------------------------*
* Work Area
*----------------------------------------------------------------------*
DATA: GW_ZFIT0064             TYPE TY_ZFIT0064,
      GW_ZFIT0069             TYPE TY_ZFIT0069,
      GW_ZFIT0069_ATIVO       TYPE TY_ZFIT0069,
      GW_ZFIT0069_PASSIVO     TYPE TY_ZFIT0069,
      GW_ZFIT0070             TYPE ZFIT0070,
      GW_ZFIT0070_ATIVO       TYPE ZFIT0070,
      GW_ZFIT0070_PASSIVO     TYPE ZFIT0070,
      GW_ZFIT0067             TYPE ZFIT0067,
      GW_ZFIT0067_ATIVO       TYPE ZFIT0067,
      GW_ZFIT0067_PASSIVO     TYPE ZFIT0067,
      GW_EXPOSICAO_DERIVATIVO TYPE TY_EXPOSICAO_DERIVATIVO,
      GW_SAIDA_DERIVATIVO     TYPE TY_SAIDA_EXPOSICAO_DERIVATIVO,
      GW_SAIDA                TYPE TY_SAIDA.

*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: GW_ACTIVE_TAB TYPE C LENGTH 50,
      VAR_BUKRS     TYPE ZFIT0064-BUKRS,
      VAR_DATA      TYPE ZFIT0064-DT_FIM_CTO.

*---------------------------------------------------------------
* Seleção.
*---------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BL01 WITH FRAME.
SELECT-OPTIONS: P_DT_FEC FOR ZFIT0064-DT_FECHAMENTO NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK BL01.

SELECTION-SCREEN BEGIN OF BLOCK BL02 WITH FRAME.
PARAMETERS:     R_NF_DF  RADIOBUTTON GROUP RB01 USER-COMMAND MUDA_TELA DEFAULT 'X'.
SELECT-OPTIONS: P_BUKRS  FOR ZFIT0064-BUKRS NO-EXTENSION NO INTERVALS.
PARAMETERS:     R_EXP_DE RADIOBUTTON GROUP RB01.
SELECTION-SCREEN END OF BLOCK BL02.


AT SELECTION-SCREEN OUTPUT.
  PERFORM: MODIFICA_TELA.

*---------------------------------------------------------------
*      Form  MODIFICA_TELA
*---------------------------------------------------------------
FORM MODIFICA_TELA .

  LOOP AT SCREEN.

    IF ( R_EXP_DE EQ 'X' ).
      CASE SCREEN-NAME.
        WHEN: 'P_BUKRS-LOW' OR '%_P_BUKRS_%_APP_%-TEXT'.
          SCREEN-INVISIBLE = 1.
          SCREEN-OUTPUT    = 0.
          SCREEN-INPUT     = 0.
          MODIFY SCREEN.
          CONTINUE.
      ENDCASE.
    ELSEIF ( R_NF_DF EQ 'X').
      SCREEN-INVISIBLE = 0.
      SCREEN-OUTPUT    = 1.
      SCREEN-INPUT     = 1.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFICA_TELA

START-OF-SELECTION.

  PERFORM: VALIDACAO_CAMPO,
           SELECIONAR_DADOS.

  IF ( R_EXP_DE IS INITIAL ).
    CALL SCREEN TELA_0100.
  ELSE.
    PERFORM: MONTAR_ALV.
    CALL SCREEN TELA_0600.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  VALIDACAO_CAMPO
*&---------------------------------------------------------------------*
FORM VALIDACAO_CAMPO .

  IF ( P_DT_FEC IS INITIAL ).
    MESSAGE 'Data de Fechamento Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF ( P_BUKRS IS INITIAL ) AND ( R_NF_DF EQ 'X' ).
    MESSAGE 'Por favor informar a empresa.' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  VAR_BUKRS = P_BUKRS-LOW.
  VAR_DATA  = P_DT_FEC-LOW.

ENDFORM.                    " VALIDACAO_CAMPO
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.

    WHEN: 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
    WHEN: 'TAB_NDF'.

      PERFORM: SELECIONAR_NDF.
      TABSTRIP-ACTIVETAB = TAB_NDF.

    WHEN: 'TAB_SWAP_VAN'.

      GW_ACTIVE_TAB = 'TAB_SWAP_VAN'.
      PERFORM: SELECIONAR_SWAP_VANILLA.
      TABSTRIP-ACTIVETAB = TAB_SWAP_VAN.

    WHEN: 'TAB_SWAP_FLUX'.

      GW_ACTIVE_TAB = 'TAB_SWAP_FLUX'.
      PERFORM: SELECIONAR_SWAP_FLUXO.
      TABSTRIP-ACTIVETAB = TAB_SWAP_FLUX.

  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  IF ( R_NF_DF EQ 'X' ).
    "Primeiro sempre vai selecionar o NDF caso o Nota Explicativa DF estiver marcado com X
    GW_ACTIVE_TAB = 'TAB_NDF'.
    PERFORM: SELECIONAR_NDF.
  ELSE.
    PERFORM: SELECIONAR_EXPOS_DERIVATIVO.
  ENDIF.
ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_NDF
*&---------------------------------------------------------------------*
FORM SELECIONAR_NDF.

  DATA: LT_ZFIT0064 TYPE TABLE OF TY_ZFIT0064,
        LW_ZFIT0064 TYPE TY_ZFIT0064.

  REFRESH: GT_ZFIT0064[], GT_SAIDA[], LT_ZFIT0064[].


  FIELD-SYMBOLS: <FS_ZFIT0064> TYPE TY_ZFIT0064.

  "MTM - Financeiro Calculo NDF
  SELECT TP_POSICAO MOEDA DT_FIM_CTO VLR_OPERACAO VLR_PRES_MTM TP_AJUSTE BANCO
     FROM ZFIT0064
    INTO TABLE GT_ZFIT0064
   WHERE BUKRS         IN P_BUKRS
     AND DT_FECHAMENTO IN P_DT_FEC.

  CHECK NOT GT_ZFIT0064[] IS INITIAL.

  LOOP AT GT_ZFIT0064 ASSIGNING <FS_ZFIT0064>.
    <FS_ZFIT0064>-DT_FIM_CTO_AUX = <FS_ZFIT0064>-DT_FIM_CTO(4).
  ENDLOOP.

  LT_ZFIT0064[] = GT_ZFIT0064[].

  LOOP AT GT_ZFIT0064 INTO GW_ZFIT0064.

    LOOP AT LT_ZFIT0064 INTO LW_ZFIT0064 WHERE TP_POSICAO       EQ GW_ZFIT0064-TP_POSICAO
                                            AND MOEDA           EQ GW_ZFIT0064-MOEDA
                                            AND DT_FIM_CTO_AUX  EQ GW_ZFIT0064-DT_FIM_CTO_AUX.

      GW_SAIDA-VLR_OPERACAO   =  GW_SAIDA-VLR_OPERACAO   + LW_ZFIT0064-VLR_OPERACAO.
      GW_SAIDA-VLR_PRES_MTM   =  GW_SAIDA-VLR_PRES_MTM   + LW_ZFIT0064-VLR_PRES_MTM.
      GW_SAIDA-VLR_PRES_MTM_R =  GW_SAIDA-VLR_PRES_MTM_R + LW_ZFIT0064-VLR_PRES_MTM.
    ENDLOOP.

    GW_SAIDA-TP_POSICAO     = GW_ZFIT0064-TP_POSICAO.
    GW_SAIDA-MOEDA          = GW_ZFIT0064-MOEDA.
    GW_SAIDA-DT_FIM_CTO     = GW_ZFIT0064-DT_FIM_CTO(4).

    DELETE GT_ZFIT0064 WHERE TP_POSICAO     EQ GW_ZFIT0064-TP_POSICAO
                         AND MOEDA          EQ GW_ZFIT0064-MOEDA
                         AND DT_FIM_CTO_AUX EQ GW_ZFIT0064-DT_FIM_CTO_AUX.

    APPEND GW_SAIDA TO GT_SAIDA.
    CLEAR: GW_SAIDA, GW_ZFIT0064, LW_ZFIT0064.
  ENDLOOP.

  CHECK NOT GT_SAIDA[] IS INITIAL.
  PERFORM: MONTAR_ALV.


ENDFORM.                    " SELECIONAR_NDF
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_SWAP_VANILLA
*&---------------------------------------------------------------------*
FORM SELECIONAR_SWAP_VANILLA.

  DATA: VAR_TABIX TYPE SY-TABIX,
        VAR_TOTAL TYPE ZFIT0064-VLR_PRES_MTM.

  DATA: TOTAL_VLR_OPERACAO     TYPE ZFIT0069-VLR_OPERACAO,
        TOTAL_VLR_PRES_MTM     TYPE ZFIT0069-VLR_MTM_P_ATIV,
        TOTAL_VLR_PRES_MTM_R   TYPE ZFIT0069-VLR_MTM_P_ATIV,
        TOTAL_VLR_OPERACAO_INT TYPE ZFIT0069-VLR_OPERACAO_INT.


  FIELD-SYMBOLS: <FS_ZFIT0069> TYPE TY_ZFIT0069.

  REFRESH: GT_ZFIT0069_ATIVO[], GT_ZFIT0069_PASSIVO[], GT_SAIDA[].
  CLEAR: GW_ZFIT0069_ATIVO, GW_ZFIT0069_PASSIVO.

  "MTM – Financeiro – SWAP VANILA
  SELECT DT_FIM_CTO VLR_OPERACAO VLR_MTM_P_ATIV VLR_MTM_P_PASSIV VLR_OPERACAO_INT BANCO
    FROM ZFIT0069
    INTO TABLE GT_ZFIT0069
  WHERE BUKRS         IN P_BUKRS
    AND DT_FECHAMENTO IN P_DT_FEC.

  CHECK NOT GT_ZFIT0069[] IS INITIAL.

  LOOP AT GT_ZFIT0069 ASSIGNING <FS_ZFIT0069>.
    <FS_ZFIT0069>-DT_FIM_CTO_AUX = <FS_ZFIT0069>-DT_FIM_CTO(4).
  ENDLOOP.
  UNASSIGN <FS_ZFIT0069>.


  GT_ZFIT0069_ATIVO[]   = GT_ZFIT0069[].
  GT_ZFIT0069_PASSIVO[] = GT_ZFIT0069[].

  "Ativa
  LOOP AT GT_ZFIT0069 INTO GW_ZFIT0069 WHERE VLR_MTM_P_ATIV IS NOT INITIAL.

    LOOP AT GT_ZFIT0069_ATIVO INTO GW_ZFIT0069_ATIVO WHERE VLR_MTM_P_ATIV IS NOT INITIAL
                                                       AND DT_FIM_CTO_AUX EQ GW_ZFIT0069-DT_FIM_CTO_AUX.

      TOTAL_VLR_OPERACAO     = TOTAL_VLR_OPERACAO     + GW_ZFIT0069_ATIVO-VLR_OPERACAO.
      TOTAL_VLR_PRES_MTM     = TOTAL_VLR_PRES_MTM     + GW_ZFIT0069_ATIVO-VLR_MTM_P_ATIV.
      TOTAL_VLR_PRES_MTM_R   = TOTAL_VLR_PRES_MTM_R   + GW_ZFIT0069_ATIVO-VLR_MTM_P_ATIV.
      TOTAL_VLR_OPERACAO_INT = TOTAL_VLR_OPERACAO_INT + GW_ZFIT0069_ATIVO-VLR_OPERACAO_INT.
    ENDLOOP.

    GW_SAIDA-VLR_OPERACAO   =   TOTAL_VLR_OPERACAO.
    GW_SAIDA-VLR_PRES_MTM   =   TOTAL_VLR_PRES_MTM   - TOTAL_VLR_OPERACAO_INT.
    GW_SAIDA-VLR_PRES_MTM_R =   TOTAL_VLR_PRES_MTM_R - TOTAL_VLR_OPERACAO_INT.



    GW_SAIDA-TP_POSICAO       = 'Ativa'.
    GW_SAIDA-MOEDA            = 'USD + Pré'.
    GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0069_ATIVO-DT_FIM_CTO(4).

    DELETE GT_ZFIT0069 WHERE VLR_MTM_P_ATIV IS NOT INITIAL
                         AND DT_FIM_CTO_AUX EQ GW_ZFIT0069-DT_FIM_CTO_AUX.

    APPEND GW_SAIDA TO GT_SAIDA.
    CLEAR: GW_SAIDA, GW_ZFIT0069_ATIVO, GW_ZFIT0069,
           TOTAL_VLR_OPERACAO,
           TOTAL_VLR_PRES_MTM,
           TOTAL_VLR_PRES_MTM_R,
           TOTAL_VLR_OPERACAO_INT.


  ENDLOOP.

  GT_ZFIT0069[] = GT_ZFIT0069_PASSIVO[].

  "Passiva
  LOOP AT GT_ZFIT0069 INTO GW_ZFIT0069  WHERE VLR_MTM_P_PASSIV IS NOT INITIAL.

    LOOP AT GT_ZFIT0069_PASSIVO INTO GW_ZFIT0069_PASSIVO WHERE VLR_MTM_P_PASSIV IS NOT INITIAL
                                                           AND DT_FIM_CTO_AUX   EQ GW_ZFIT0069-DT_FIM_CTO_AUX.

      TOTAL_VLR_OPERACAO      = TOTAL_VLR_OPERACAO      + GW_ZFIT0069_PASSIVO-VLR_OPERACAO.
      TOTAL_VLR_PRES_MTM      = TOTAL_VLR_PRES_MTM      + GW_ZFIT0069_PASSIVO-VLR_MTM_P_PASSIV.
      TOTAL_VLR_PRES_MTM_R    = TOTAL_VLR_PRES_MTM_R    + GW_ZFIT0069_PASSIVO-VLR_MTM_P_PASSIV.
      TOTAL_VLR_OPERACAO_INT  = TOTAL_VLR_OPERACAO_INT  + GW_ZFIT0069_PASSIVO-VLR_OPERACAO_INT.

    ENDLOOP.

    GW_SAIDA-VLR_OPERACAO   =   TOTAL_VLR_OPERACAO.
    GW_SAIDA-VLR_PRES_MTM   =   ( TOTAL_VLR_PRES_MTM   - TOTAL_VLR_OPERACAO_INT ) * -1.
    GW_SAIDA-VLR_PRES_MTM_R =   ( TOTAL_VLR_PRES_MTM_R - TOTAL_VLR_OPERACAO_INT ) * -1.

    GW_SAIDA-TP_POSICAO       = 'Passiva'.
    GW_SAIDA-MOEDA            = '% CDI'.
    GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0069_PASSIVO-DT_FIM_CTO(4).

    DELETE GT_ZFIT0069 WHERE VLR_MTM_P_PASSIV IS NOT INITIAL
                         AND DT_FIM_CTO_AUX   EQ GW_ZFIT0069-DT_FIM_CTO_AUX.

    APPEND GW_SAIDA TO GT_SAIDA.
    CLEAR: GW_SAIDA, GW_ZFIT0069_PASSIVO, GW_ZFIT0069,
           TOTAL_VLR_OPERACAO,
           TOTAL_VLR_PRES_MTM,
           TOTAL_VLR_PRES_MTM_R,
           TOTAL_VLR_OPERACAO_INT.

  ENDLOOP.

  CHECK NOT GT_SAIDA[] IS INITIAL.

  PERFORM: MONTAR_ALV.


ENDFORM.                    " SELECIONAR_SWAP_VANILLA
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_SWAP_FLUXO
*&---------------------------------------------------------------------*
FORM SELECIONAR_SWAP_FLUXO.

  DATA: LT_SAIDA     TYPE TABLE OF TY_SAIDA,
        LW_SAIDA     TYPE TY_SAIDA.


  DATA: TOTAL_VLR_OPERACAO TYPE ZFIT0070-VLR_OPERACAO,
        TOTAL_VLR_PRES_MTM TYPE ZFIT0070-VLR_MTM_P_ATIV.

  REFRESH: GT_ZFIT0070[], GT_ZFIT0067[], GT_SAIDA[].

  SELECT * FROM ZFIT0070
    INTO TABLE GT_ZFIT0070
  WHERE BUKRS         IN P_BUKRS
    AND DT_FECHAMENTO IN P_DT_FEC
    AND COD_OPER      EQ 'V'.

  IF ( SY-SUBRC EQ 0 ).

    GT_ZFIT0070_ATIVO[]   = GT_ZFIT0070[].
    GT_ZFIT0070_PASSIVO[] = GT_ZFIT0070[].

    "Ativo
    LOOP AT GT_ZFIT0070_ATIVO INTO GW_ZFIT0070_ATIVO WHERE VLR_MTM_P_ATIV IS NOT INITIAL.

      CASE GW_ZFIT0070_ATIVO-TX_JRS_ALEM_CDI.
        WHEN: '0'.

          GW_SAIDA-TP_POSICAO       = 'Ativa'.
          GW_SAIDA-MOEDA            = '% CDI'.
          GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0070_ATIVO-DT_FIM_CTO(4).
          GW_SAIDA-VLR_OPERACAO     = GW_ZFIT0070_ATIVO-VLR_OPERACAO.
          GW_SAIDA-VLR_PRES_MTM     = GW_ZFIT0070_ATIVO-VLR_MTM_P_ATIV.
          GW_SAIDA-VLR_PRES_MTM_R   = GW_ZFIT0070_ATIVO-VLR_MTM_P_ATIV.

        WHEN OTHERS.

          GW_SAIDA-TP_POSICAO       = 'Ativa'.
          GW_SAIDA-MOEDA            = 'CDI + Pré'.
          GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0070_ATIVO-DT_FIM_CTO(4).
          GW_SAIDA-VLR_OPERACAO     = GW_ZFIT0070_ATIVO-VLR_OPERACAO.
          GW_SAIDA-VLR_PRES_MTM     = GW_ZFIT0070_ATIVO-VLR_MTM_P_ATIV.
          GW_SAIDA-VLR_PRES_MTM_R   = GW_ZFIT0070_ATIVO-VLR_MTM_P_ATIV.

      ENDCASE.

      APPEND GW_SAIDA TO GT_SAIDA.
      CLEAR: GW_SAIDA, GW_ZFIT0070_ATIVO.

    ENDLOOP.

    "Passivo
    LOOP AT GT_ZFIT0070_PASSIVO INTO GW_ZFIT0070_PASSIVO WHERE VLR_MTM_P_PASSIV IS NOT INITIAL.

      CASE GW_ZFIT0070_ATIVO-TX_JRS_ALEM_CDI.
        WHEN: '0'.

          GW_SAIDA-TP_POSICAO       = 'Passiva'.
          GW_SAIDA-MOEDA            = 'Dolár + Pré'.
          GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0070_PASSIVO-DT_FIM_CTO(4).
          GW_SAIDA-VLR_OPERACAO     = GW_ZFIT0070_PASSIVO-VLR_OPERACAO.
          GW_SAIDA-VLR_PRES_MTM     = GW_ZFIT0070_PASSIVO-VLR_MTM_P_PASSIV.
          GW_SAIDA-VLR_PRES_MTM_R   = GW_ZFIT0070_PASSIVO-VLR_MTM_P_PASSIV.

        WHEN OTHERS.

          GW_SAIDA-TP_POSICAO       = 'Passiva'.
          GW_SAIDA-MOEDA            = 'Dolár + Pré'.
          GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0070_PASSIVO-DT_FIM_CTO(4).
          GW_SAIDA-VLR_OPERACAO     = GW_ZFIT0070_PASSIVO-VLR_OPERACAO.
          GW_SAIDA-VLR_PRES_MTM     = GW_ZFIT0070_PASSIVO-VLR_MTM_P_PASSIV.
          GW_SAIDA-VLR_PRES_MTM_R   = GW_ZFIT0070_PASSIVO-VLR_MTM_P_PASSIV.

      ENDCASE.

      APPEND GW_SAIDA TO GT_SAIDA.
      CLEAR: GW_SAIDA, GW_ZFIT0070_PASSIVO .

    ENDLOOP.

  ENDIF.

  SELECT * FROM ZFIT0067
    INTO TABLE GT_ZFIT0067
  WHERE BUKRS         IN P_BUKRS
    AND DT_FECHAMENTO IN P_DT_FEC
    AND COD_OPER      EQ 'S'.

  IF ( SY-SUBRC EQ 0 ).

    GT_ZFIT0067_ATIVO[] = GT_ZFIT0067[].

    "Ativo
    LOOP AT GT_ZFIT0067_ATIVO INTO GW_ZFIT0067_ATIVO WHERE VLR_MTM_P_ATIV IS NOT INITIAL.

      GW_SAIDA-TP_POSICAO       = 'Ativa'.
      GW_SAIDA-MOEDA            = 'Pré'.
      GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0067_ATIVO-DT_FIM_CTO(4).
      GW_SAIDA-VLR_OPERACAO     = GW_ZFIT0067_ATIVO-VLR_PARC.
      GW_SAIDA-VLR_PRES_MTM     = GW_ZFIT0067_ATIVO-VLR_MTM_P_ATIV.
      GW_SAIDA-VLR_PRES_MTM_R   = GW_ZFIT0067_ATIVO-VLR_MTM_P_ATIV.

      APPEND GW_SAIDA TO GT_SAIDA.
      CLEAR: GW_SAIDA, GW_ZFIT0067_ATIVO.

    ENDLOOP.

    GT_ZFIT0067_PASSIVO[] = GT_ZFIT0067[].

    "Passivo
    LOOP AT GT_ZFIT0067_PASSIVO INTO GW_ZFIT0067_PASSIVO WHERE VLR_MTM_P_PASSIV IS NOT INITIAL.

      GW_SAIDA-TP_POSICAO       = 'Passiva'.
      GW_SAIDA-MOEDA            = 'Dolár + Pré'.
      GW_SAIDA-DT_FIM_CTO       = GW_ZFIT0067_PASSIVO-DT_FIM_CTO(4).
      GW_SAIDA-VLR_OPERACAO     = GW_ZFIT0067_PASSIVO-VLR_PARC.
      GW_SAIDA-VLR_PRES_MTM     = GW_ZFIT0067_PASSIVO-VLR_MTM_P_PASSIV.
      GW_SAIDA-VLR_PRES_MTM_R   = GW_ZFIT0067_PASSIVO-VLR_MTM_P_PASSIV.

      APPEND GW_SAIDA TO GT_SAIDA.
      CLEAR: GW_SAIDA, GW_ZFIT0067_PASSIVO.
    ENDLOOP.
  ENDIF.

  CHECK NOT GT_SAIDA[] IS INITIAL.

  "Somar todos agrupando moeda/dt_fim_cto.
  LT_SAIDA[] = GT_SAIDA[].

  LOOP AT GT_SAIDA INTO GW_SAIDA WHERE CHECK NE 'S'.

    LOOP AT LT_SAIDA INTO LW_SAIDA WHERE MOEDA      EQ GW_SAIDA-MOEDA
                                     AND DT_FIM_CTO EQ GW_SAIDA-DT_FIM_CTO.

      TOTAL_VLR_OPERACAO = TOTAL_VLR_OPERACAO + LW_SAIDA-VLR_OPERACAO.
      TOTAL_VLR_PRES_MTM = TOTAL_VLR_PRES_MTM + LW_SAIDA-VLR_PRES_MTM.
    ENDLOOP.

    DELETE GT_SAIDA WHERE MOEDA      EQ GW_SAIDA-MOEDA
                      AND DT_FIM_CTO EQ GW_SAIDA-DT_FIM_CTO.

    GW_SAIDA-TP_POSICAO       = GW_SAIDA-TP_POSICAO.

    GW_SAIDA-MOEDA            = GW_SAIDA-MOEDA.
    GW_SAIDA-DT_FIM_CTO       = GW_SAIDA-DT_FIM_CTO.
    GW_SAIDA-VLR_OPERACAO     = TOTAL_VLR_OPERACAO.


    CASE GW_SAIDA-TP_POSICAO.
      WHEN: 'Passiva'.
        GW_SAIDA-VLR_PRES_MTM     = TOTAL_VLR_PRES_MTM * -1.
        GW_SAIDA-VLR_PRES_MTM_R   = TOTAL_VLR_PRES_MTM * -1.
      WHEN OTHERS.
        GW_SAIDA-VLR_PRES_MTM     = TOTAL_VLR_PRES_MTM.
        GW_SAIDA-VLR_PRES_MTM_R   = TOTAL_VLR_PRES_MTM.
    ENDCASE.

    GW_SAIDA-CHECK            = 'S'.

    APPEND GW_SAIDA TO GT_SAIDA.
    CLEAR: GW_SAIDA, TOTAL_VLR_OPERACAO, TOTAL_VLR_PRES_MTM.

  ENDLOOP.

  DELETE GT_SAIDA WHERE CHECK NE 'S'.
  SORT: GT_SAIDA BY TP_POSICAO DESCENDING.

  PERFORM: MONTAR_ALV.


ENDFORM.                    " SELECIONAR_SWAP_FLUXO
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_EXPOS_DERIVATIVO
*&---------------------------------------------------------------------*
FORM SELECIONAR_EXPOS_DERIVATIVO .

  DATA: GT_ZFIT0064_AUX TYPE TABLE OF TY_ZFIT0064,
        GW_ZFIT0064_AUX TYPE TY_ZFIT0064.

  DATA: GT_ZFIT0069_AUX TYPE TABLE OF TY_ZFIT0069,
        GW_ZFIT0069_AUX TYPE TY_ZFIT0069.

  DATA: GT_ZFIT0067_AUX TYPE TABLE OF ZFIT0067,
        GW_ZFIT0067_AUX TYPE ZFIT0067.

  DATA: GT_ZFIT0070_AUX TYPE TABLE OF ZFIT0070,
        GW_ZFIT0070_AUX TYPE ZFIT0070.

  DATA: GT_EXPOSICAO_DERIVATIVO_AUX TYPE TABLE OF TY_EXPOSICAO_DERIVATIVO,
        GW_EXPOSICAO_DERIVATIVO_AUX TYPE TY_EXPOSICAO_DERIVATIVO.

  REFRESH: GT_ZFIT0064[], GT_ZFIT0067[], GT_ZFIT0069[], GT_ZFIT0070.

  "MTM - Financeiro Calculo NDF
  SELECT TP_POSICAO MOEDA DT_FIM_CTO VLR_OPERACAO VLR_PRES_MTM TP_AJUSTE BANCO
     FROM ZFIT0064
    INTO TABLE GT_ZFIT0064
   WHERE DT_FECHAMENTO IN P_DT_FEC.

  "Separar todos que são NDF valores ativos e passivos, já somados POR BANCO E TP_AJUSTE
  GT_ZFIT0064_AUX[] = GT_ZFIT0064[].
  LOOP AT GT_ZFIT0064 INTO GW_ZFIT0064.
    LOOP AT GT_ZFIT0064_AUX INTO GW_ZFIT0064_AUX WHERE TP_AJUSTE EQ GW_ZFIT0064-TP_AJUSTE
                                                   AND BANCO     EQ GW_ZFIT0064-BANCO.
      GW_EXPOSICAO_DERIVATIVO-VALOR     = GW_EXPOSICAO_DERIVATIVO-VALOR + GW_ZFIT0064_AUX-VLR_PRES_MTM.
    ENDLOOP.

    DELETE GT_ZFIT0064 WHERE TP_AJUSTE EQ GW_ZFIT0064-TP_AJUSTE
                         AND BANCO     EQ GW_ZFIT0064-BANCO.

    GW_EXPOSICAO_DERIVATIVO-TIPO      = 'NDF'.
    GW_EXPOSICAO_DERIVATIVO-TP_AJUSTE = GW_ZFIT0064-TP_AJUSTE.
    GW_EXPOSICAO_DERIVATIVO-BANCO     = GW_ZFIT0064-BANCO.

    APPEND GW_EXPOSICAO_DERIVATIVO TO GT_EXPOSICAO_DERIVATIVO.
    CLEAR: GW_ZFIT0064, GW_EXPOSICAO_DERIVATIVO, GW_ZFIT0064_AUX.
  ENDLOOP.

  "MTM – Financeiro – SWAP VANILA
  SELECT DT_FIM_CTO VLR_OPERACAO VLR_MTM_P_ATIV VLR_MTM_P_PASSIV VLR_OPERACAO_INT TP_AJUSTE BANCO VLR_AJ_MERC
      FROM ZFIT0069
      INTO TABLE GT_ZFIT0069
    WHERE DT_FECHAMENTO IN P_DT_FEC.

  GT_ZFIT0069_AUX[] = GT_ZFIT0069[].

  LOOP AT GT_ZFIT0069 INTO GW_ZFIT0069.
    LOOP AT GT_ZFIT0069_AUX INTO GW_ZFIT0069_AUX WHERE TP_AJUSTE EQ GW_ZFIT0069-TP_AJUSTE
                                                   AND BANCO     EQ GW_ZFIT0069-BANCO.
      GW_EXPOSICAO_DERIVATIVO-VALOR = GW_EXPOSICAO_DERIVATIVO-VALOR + GW_ZFIT0069-VLR_AJ_MERC.
    ENDLOOP.

    DELETE GT_ZFIT0069  WHERE TP_AJUSTE EQ GW_ZFIT0069-TP_AJUSTE
                          AND BANCO     EQ GW_ZFIT0069-BANCO.

    GW_EXPOSICAO_DERIVATIVO-TIPO      = 'SWAPV'. "Swap Vanilla
    GW_EXPOSICAO_DERIVATIVO-TP_AJUSTE = GW_ZFIT0069-TP_AJUSTE.
    GW_EXPOSICAO_DERIVATIVO-BANCO     = GW_ZFIT0069-BANCO.

    APPEND GW_EXPOSICAO_DERIVATIVO TO GT_EXPOSICAO_DERIVATIVO.
    CLEAR: GW_EXPOSICAO_DERIVATIVO, GW_ZFIT0069, GW_ZFIT0069_AUX.
  ENDLOOP.

  "MTM – Financeiro Calculo SWAP Com CDI
  SELECT * FROM ZFIT0070
    INTO TABLE GT_ZFIT0070
  WHERE DT_FECHAMENTO IN P_DT_FEC
    AND COD_OPER      EQ 'V'.

  GT_ZFIT0070_AUX[] = GT_ZFIT0070[].
  LOOP AT GT_ZFIT0070 INTO GW_ZFIT0070.
    LOOP AT GT_ZFIT0070_AUX INTO GW_ZFIT0070_AUX WHERE TP_AJUSTE EQ GW_ZFIT0070-TP_AJUSTE
                                                   AND BANCO     EQ GW_ZFIT0070-BANCO.
      GW_EXPOSICAO_DERIVATIVO-VALOR = GW_EXPOSICAO_DERIVATIVO-VALOR + GW_ZFIT0070_AUX-VLR_AJ_MERC.
    ENDLOOP.

    DELETE GT_ZFIT0070 WHERE TP_AJUSTE EQ GW_ZFIT0070-TP_AJUSTE
                         AND BANCO     EQ GW_ZFIT0070-BANCO.

    GW_EXPOSICAO_DERIVATIVO-TIPO      = 'SWAPF'. "Swap Fluxo
    GW_EXPOSICAO_DERIVATIVO-TP_AJUSTE = GW_ZFIT0070-TP_AJUSTE.
    GW_EXPOSICAO_DERIVATIVO-BANCO     = GW_ZFIT0070-BANCO.

    APPEND GW_EXPOSICAO_DERIVATIVO TO GT_EXPOSICAO_DERIVATIVO.
    CLEAR: GW_EXPOSICAO_DERIVATIVO, GW_ZFIT0070, GW_ZFIT0070_AUX.
  ENDLOOP.

  "MTM – Financeiro Calculo SWAP
  SELECT * FROM ZFIT0067
    INTO TABLE GT_ZFIT0067
  WHERE DT_FECHAMENTO IN P_DT_FEC
    AND COD_OPER      EQ 'S'.

  GT_ZFIT0067_AUX[] = GT_ZFIT0067[].
  LOOP AT GT_ZFIT0067 INTO GW_ZFIT0067.
    LOOP AT GT_ZFIT0067_AUX INTO GW_ZFIT0067_AUX WHERE TP_AJUSTE EQ GW_ZFIT0067-TP_AJUSTE
                                                   AND BANCO     EQ GW_ZFIT0067-BANCO.

      GW_EXPOSICAO_DERIVATIVO-VALOR = GW_EXPOSICAO_DERIVATIVO-VALOR + GW_ZFIT0067_AUX-VLR_AJ_MERC.

    ENDLOOP.

    DELETE GT_ZFIT0067 WHERE TP_AJUSTE EQ GW_ZFIT0067-TP_AJUSTE
                         AND BANCO     EQ GW_ZFIT0067-BANCO.

    GW_EXPOSICAO_DERIVATIVO-TIPO      = 'SWAPF'. "Swap Fluxo
    GW_EXPOSICAO_DERIVATIVO-TP_AJUSTE = GW_ZFIT0067-TP_AJUSTE.
    GW_EXPOSICAO_DERIVATIVO-BANCO     = GW_ZFIT0067-BANCO.

    APPEND GW_EXPOSICAO_DERIVATIVO TO GT_EXPOSICAO_DERIVATIVO.
    CLEAR: GW_EXPOSICAO_DERIVATIVO, GW_ZFIT0070, GW_ZFIT0070_AUX.
  ENDLOOP.

  CHECK NOT GT_EXPOSICAO_DERIVATIVO[] IS INITIAL.

  REFRESH: GT_ZFIT0064_AUX[],
           GT_ZFIT0069_AUX[],
           GT_ZFIT0067_AUX[],
           GT_ZFIT0070_AUX[].

  GT_EXPOSICAO_DERIVATIVO_AUX[] = GT_EXPOSICAO_DERIVATIVO[].
  LOOP AT GT_EXPOSICAO_DERIVATIVO INTO GW_EXPOSICAO_DERIVATIVO.

    LOOP AT GT_EXPOSICAO_DERIVATIVO_AUX INTO GW_EXPOSICAO_DERIVATIVO_AUX WHERE BANCO EQ GW_EXPOSICAO_DERIVATIVO-BANCO.

      CASE GW_EXPOSICAO_DERIVATIVO_AUX-TIPO.

        WHEN: 'NDF'.
          IF ( GW_EXPOSICAO_DERIVATIVO_AUX-TP_AJUSTE EQ 'ATIVO' ).
            GW_SAIDA_DERIVATIVO-VLR_ATIVO_NDF    = GW_SAIDA_DERIVATIVO-VLR_ATIVO_NDF + GW_EXPOSICAO_DERIVATIVO_AUX-VALOR.
          ELSE.
            GW_SAIDA_DERIVATIVO-VLR_PASSIVO_NDF  = GW_SAIDA_DERIVATIVO-VLR_ATIVO_NDF + GW_EXPOSICAO_DERIVATIVO_AUX-VALOR.
          ENDIF.

        WHEN: 'SWAPV'.
          IF ( GW_EXPOSICAO_DERIVATIVO_AUX-TP_AJUSTE EQ 'ATIVO' ).
            GW_SAIDA_DERIVATIVO-VLR_ATIVO_SWAPV   = GW_SAIDA_DERIVATIVO-VLR_ATIVO_SWAPV + GW_EXPOSICAO_DERIVATIVO_AUX-VALOR.

          ELSE.
            GW_SAIDA_DERIVATIVO-VLR_PASSIVO_SWAPV = GW_SAIDA_DERIVATIVO-VLR_ATIVO_SWAPV + GW_EXPOSICAO_DERIVATIVO_AUX-VALOR.
          ENDIF.

        WHEN: 'SWAPF'.

          IF ( GW_EXPOSICAO_DERIVATIVO_AUX-TP_AJUSTE EQ 'ATIVO' ).
            GW_SAIDA_DERIVATIVO-VLR_ATIVO_SWAPF = GW_SAIDA_DERIVATIVO-VLR_ATIVO_SWAPF + GW_EXPOSICAO_DERIVATIVO_AUX-VALOR.
          ELSE.
            GW_SAIDA_DERIVATIVO-VLR_PASSIVO_SWAPF = GW_SAIDA_DERIVATIVO-VLR_PASSIVO_SWAPF + GW_EXPOSICAO_DERIVATIVO_AUX-VALOR.

          ENDIF.
      ENDCASE.
    ENDLOOP.

    GW_SAIDA_DERIVATIVO-TOTAL_ATIVO   = GW_SAIDA_DERIVATIVO-VLR_ATIVO_NDF   + GW_SAIDA_DERIVATIVO-VLR_ATIVO_SWAPV   + GW_SAIDA_DERIVATIVO-VLR_ATIVO_SWAPF.
    GW_SAIDA_DERIVATIVO-TOTAL_PASSIVO = GW_SAIDA_DERIVATIVO-VLR_PASSIVO_NDF + GW_SAIDA_DERIVATIVO-VLR_PASSIVO_SWAPV + GW_SAIDA_DERIVATIVO-VLR_PASSIVO_SWAPF.
    GW_SAIDA_DERIVATIVO-SALDO         = GW_SAIDA_DERIVATIVO-TOTAL_ATIVO - GW_SAIDA_DERIVATIVO-TOTAL_PASSIVO.


    DELETE GT_EXPOSICAO_DERIVATIVO WHERE BANCO EQ GW_EXPOSICAO_DERIVATIVO-BANCO.

    GW_SAIDA_DERIVATIVO-BANCO = GW_EXPOSICAO_DERIVATIVO-BANCO.

    APPEND GW_SAIDA_DERIVATIVO TO GT_SAIDA_DERIVATIVO.

    CLEAR: GW_SAIDA_DERIVATIVO, GW_EXPOSICAO_DERIVATIVO, GW_EXPOSICAO_DERIVATIVO_AUX.
  ENDLOOP.


ENDFORM.                    " SELECIONAR_EXPOS_DERIVATIVO
*&---------------------------------------------------------------------*
*&      Module  PBO_0600  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0600 OUTPUT.
  SET PF-STATUS 'FF0600'.
  SET TITLEBAR  'TB0600'.
ENDMODULE.                 " PBO_0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0600  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0600 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " PAI_0600  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ALV
*&---------------------------------------------------------------------*
FORM MONTAR_ALV .

  FREE: OBJ_CONTAINER, OBJ_ALV.
  REFRESH: GT_FIELDCATALOG[].

  DATA: WL_LAYOUT  TYPE LVC_S_LAYO,
        WL_VARIANT TYPE DISVARIANT.

  DATA: CONTAINER_NAME TYPE C LENGTH 100.

  IF ( R_NF_DF EQ 'X' ).
    CASE GW_ACTIVE_TAB.
      WHEN: 'TAB_NDF'.
        CONTAINER_NAME = 'CONTAINER_NDF'.
      WHEN: 'TAB_SWAP_VAN'.
        CONTAINER_NAME = 'CONTAINER_SWAP_VAN'.
      WHEN: 'TAB_SWAP_FLUX'.
        CONTAINER_NAME = 'CONTAINER_SWAP_FLUX'.
    ENDCASE.
  ELSE.
    CONTAINER_NAME = 'CONTAINER_EX_DERI'.
  ENDIF.

  CONDENSE CONTAINER_NAME NO-GAPS.

  CREATE OBJECT OBJ_CONTAINER
    EXPORTING
      CONTAINER_NAME              = CONTAINER_NAME
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  PERFORM: MONTAR_CAGALOG.

  CREATE OBJECT OBJ_ALV
    EXPORTING
      I_PARENT          = OBJ_CONTAINER
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  IF ( R_NF_DF EQ 'X' ).

    CALL METHOD OBJ_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WL_LAYOUT
        IS_VARIANT                    = WL_VARIANT
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = GT_SAIDA[]
        IT_FIELDCATALOG               = GT_FIELDCATALOG[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD OBJ_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WL_LAYOUT
        IS_VARIANT                    = WL_VARIANT
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = GT_SAIDA_DERIVATIVO[]
        IT_FIELDCATALOG               = GT_FIELDCATALOG[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.                    " MONTAR_ALV
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CAGALOG
*&---------------------------------------------------------------------*
FORM MONTAR_CAGALOG .

  IF ( R_NF_DF EQ 'X' ).

    CASE GW_ACTIVE_TAB.

      WHEN: 'TAB_NDF'.

        PERFORM FIELDCATALOG USING:

               'TP_POSICAO'      'Posição'                       '15'  ' ' '' ' ' '' ' ',
               'MOEDA'           'Tipo de Ativo'                 '15'  ' ' '' ' ' '' ' ',
               'DT_FIM_CTO'      'Vencimento'                    '15'  ' ' '' ' ' '' ' ',
               'VLR_OPERACAO'    'Valor de Referência (USD MM)'  '30'  ' ' '' ' ' '' 'X',
               'VLR_PRES_MTM'    'Valor Justo no Balanço'        '20'  ' ' '' ' ' '' 'X',
               'VLR_PRES_MTM_R'  'Efeito no Resultado'           '20'  ' ' '' ' ' '' 'X'.

      WHEN: 'TAB_SWAP_VAN'.

        PERFORM FIELDCATALOG USING:

               'TP_POSICAO'      'Posição'                       '15'  ' ' '' ' ' '' ' ',
               'MOEDA'           'Tipo de Ativo'                 '15'  ' ' '' ' ' '' ' ',
               'DT_FIM_CTO'      'Vencimento'                    '15'  ' ' '' ' ' '' ' ',
               'VLR_OPERACAO'    'Valor de Referência (USD MM)'  '30'  ' ' '' ' ' '' 'X',
               'VLR_PRES_MTM'    'Valor Justo no Balanço'        '20'  ' ' '' ' ' '' 'X',
               'VLR_PRES_MTM_R'  'Efeito no Resultado'           '20'  ' ' '' ' ' '' 'X'.

      WHEN: 'TAB_SWAP_FLUX'.

        PERFORM FIELDCATALOG USING:

               'TP_POSICAO'      'Posição'                       '15'  ' ' '' ' ' '' ' ',
               'MOEDA'           'Tipo de Ativo'                 '15'  ' ' '' ' ' '' ' ',
               'DT_FIM_CTO'      'Vencimento'                    '15'  ' ' '' ' ' '' ' ',
               'VLR_OPERACAO'    'Valor de Referência (USD MM)'  '30'  ' ' '' ' ' '' 'X',
               'VLR_PRES_MTM'    'Valor Justo no Balanço'        '20'  ' ' '' ' ' '' 'X',
               'VLR_PRES_MTM_R'  'Efeito no Resultado'           '20'  ' ' '' ' ' '' 'X'.
    ENDCASE.

  ELSE.

    PERFORM FIELDCATALOG USING:
          'BANCO'               'Banco'                '15'  ' ' '' ' ' '' 'X',
          'VLR_ATIVO_NDF'       'Ativo NDF'            '20'  ' ' '' ' ' '' 'X',
          'VLR_PASSIVO_NDF'     'Passivo NDF'          '20'  ' ' '' ' ' '' 'X',
          'VLR_ATIVO_SWAPV'     'Ativo SWAP Vanilla'   '30'  ' ' '' ' ' '' 'X',
          'VLR_PASSIVO_SWAPV'   'Passivo SWAP Vanilla' '30'  ' ' '' ' ' '' 'X',
          'VLR_ATIVO_SWAPF'     'Ativo SWAP Fluxo'     '20'  ' ' '' ' ' '' 'X',
          'VLR_PASSIVO_SWAPF'   'Passivo SWAP FLuxo'   '20'  ' ' '' ' ' '' 'X',

          'TOTAL_ATIVO'         'Total Ativo'          '20'  ' ' '' ' ' '' 'X',
          'TOTAL_PASSIVO'       'Total Passivo'        '20'  ' ' '' ' ' '' 'X',
          'SALDO'               'Saldo'                '15'  ' ' '' ' ' '' 'X'.
  ENDIF.

ENDFORM.                    " MONTAR_CAGALOG
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FIELDCATALOG  USING    VALUE(P_FIELDNAME)
                            VALUE(P_DESC)
                            VALUE(P_TAM)
                            VALUE(P_NO_ZERO)
                            VALUE(P_HOTSPOT)
                            VALUE(P_COR)
                            VALUE(P_JUST)
                            VALUE(P_SUM).

  GW_FIELDCATALOG-FIELDNAME = P_FIELDNAME.
  GW_FIELDCATALOG-SCRTEXT_L = P_DESC.
  GW_FIELDCATALOG-SCRTEXT_M = P_DESC.
  GW_FIELDCATALOG-SCRTEXT_S = P_DESC.
  GW_FIELDCATALOG-OUTPUTLEN = P_TAM.
  GW_FIELDCATALOG-NO_ZERO   = P_NO_ZERO.
  GW_FIELDCATALOG-HOTSPOT   = P_HOTSPOT.
  GW_FIELDCATALOG-EMPHASIZE = P_COR.
  GW_FIELDCATALOG-JUST      = P_JUST.
  GW_FIELDCATALOG-DO_SUM    = P_SUM.

  APPEND GW_FIELDCATALOG TO GT_FIELDCATALOG.

  CLEAR: GW_FIELDCATALOG.

ENDFORM.                    " FIELDCATALOG
