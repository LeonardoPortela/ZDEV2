*&---------------------------------------------------------------------*
*& Report  ZMEMO0001                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&********************* Histórico de Alterações ************************
*& Autor            Data          Request       Descrição              *
*&---------------------------------------------------------------------*
*& Marcos Faneli    03.06.2014                  CH.126368              *
*&---------------------------------------------------------------------*

REPORT  ZMEMO0001.

TABLES: ZDOC_MEMORANDO,
        ZDOC_MEMO_NF_EXP.

TYPE-POOLS: ICON.

TYPES: BEGIN OF TP_REGISTRO_DET.
        INCLUDE STRUCTURE ZEXPORT_NOTAS.
TYPES: "Memorando Notas de Produtor
       NR_MEMORANDO    TYPE Z_MEMORANDO,
       QTD_VINC_MEMO   TYPE J_1BNETQTY,
       "Memorando Informações do Memorando
       REMETENTE       TYPE LIFNR,
       NUMERO_MEMO     TYPE Z_MEMO_NUMERO,
       NR_DDE          TYPE ZNR_DDE_EX,
       DT_DDE          TYPE J_1BDOCDAT,
       NR_RE           TYPE ZNR_REG_EX,
       DT_RE           TYPE J_1BDOCDAT,
       "Nota Fiscal de Exportação
       EXP_DOCNUM      TYPE J_1BDOCNUM,
       EXP_SERIE       TYPE J_1BSERIES,
       EXP_NUMERO      TYPE J_1BNFNUM9,
       EXP_DT_EMISSAO  TYPE J_1BDOCDAT,
       EXP_QUANTIDADE  TYPE J_1BNETQTY,
       "Fatura/Remessa da nota fiscal de exportação.
       VBELN           TYPE VBELN_VF,
       VGBEL           TYPE VGBEL,
       AUBEL           TYPE VBELN_VA,
       POSSUI_ZSTD0014 TYPE CHAR1,
       STATUS_MEMO     TYPE CHAR04,
       STATUS_MM       TYPE Z_MEMO_STATUS.
TYPES: END OF TP_REGISTRO_DET.

DATA: IT_SAIDA      TYPE TABLE OF TP_REGISTRO_DET WITH HEADER LINE,
      IT_NF_MEMO    TYPE TABLE OF ZDOC_MEMO_NOTA   WITH HEADER LINE,
      OK_CODE       TYPE SY-UCOMM,
      PRIM_ALV      TYPE C LENGTH 1,
      CATALOGO_ALV  TYPE LVC_T_FCAT,
      CONTAINER_ALV TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV           TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAYOUT     TYPE LVC_S_LAYO,
      SCROLL_COL    TYPE LVC_S_COL,
      SCROLL_ROW    TYPE LVC_S_ROID.

* Menssagem de erro
*------------------------------------------------------------------------------*
DATA: LV         TYPE        I,
      LV_MESSAGE TYPE        STRING,
      LR_EXC     TYPE REF TO CX_ROOT.

SELECTION-SCREEN: BEGIN OF BLOCK BL_001 WITH FRAME TITLE TEXT-010.

PARAMETERS: P_EMPRES TYPE ZDOC_MEMORANDO-BUKRS OBLIGATORY,
            P_CENTRO TYPE T001W-WERKS OBLIGATORY,
            P_MATER  TYPE MARA-MATNR OBLIGATORY,
            P_REMET  TYPE ZDOC_MEMORANDO-REMETENTE.
SELECT-OPTIONS P_DTNOTA FOR ZDOC_MEMO_NF_EXP-DT_EMISSAO_NOTA.
SELECTION-SCREEN: END OF BLOCK BL_001.

INITIALIZATION.
  PRIM_ALV = SPACE.

START-OF-SELECTION.

  PERFORM PESQUISAR.

  CALL SCREEN 0001.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.

  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  PERFORM CRIA_ALV.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001_EXT INPUT.

  CASE OK_CODE.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001_EXT  INPUT

*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM Z_ESTRUTURA_FIELDCAT TABLES IT_CATALOGO TYPE LVC_T_FCAT
                           USING P_TAB_NAME
                                 P_FIELDNAME
                                 P_TEXTO_GRANDE
                                 P_HOT
                                 P_POSICAO
                                 P_OUTPUTLEN
                                 P_FIX_COLUMN
                                 P_CONVEXIT
                                 P_DO_SUM
                                 P_ICON
                                 P_JUST
                                 P_EMPHASIZE
                                 P_EDIT.
  DATA CATALOG TYPE LVC_S_FCAT.
  CLEAR CATALOG.
  CATALOG-TABNAME     = P_TAB_NAME.
  CATALOG-FIELDNAME   = P_FIELDNAME.
  CATALOG-SCRTEXT_L   = P_TEXTO_GRANDE.
  CATALOG-SCRTEXT_M   = P_TEXTO_GRANDE.
  CATALOG-SCRTEXT_S   = P_TEXTO_GRANDE.
  CATALOG-HOTSPOT     = P_HOT.
  CATALOG-COL_POS     = P_POSICAO.
  CATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  CATALOG-FIX_COLUMN  = P_FIX_COLUMN.
  CATALOG-CONVEXIT    = P_CONVEXIT.
  CATALOG-DO_SUM      = P_DO_SUM.
  CATALOG-ICON        = P_ICON.
  CATALOG-JUST        = P_JUST.
  CATALOG-EMPHASIZE   = P_EMPHASIZE.
  CATALOG-EDIT        = P_EDIT.
  APPEND CATALOG TO IT_CATALOGO.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CRIA_ALV .

  CONSTANTS: TABELA TYPE STRING VALUE 'IT_SAIDA'.

  DATA: TEXT_N001 TYPE C LENGTH 50 VALUE 'ZSTD0014',
        TEXT_N002 TYPE C LENGTH 50 VALUE 'Remessa',
        TEXT_N003 TYPE C LENGTH 50 VALUE 'Fatura',

        TEXT_N004 TYPE C LENGTH 50 VALUE 'Exp. Doc. NF',
        TEXT_N005 TYPE C LENGTH 50 VALUE 'Exp. Número',
        TEXT_N006 TYPE C LENGTH 50 VALUE 'Exp. Série',
        TEXT_N007 TYPE C LENGTH 50 VALUE 'Exp. Data',
        TEXT_N008 TYPE C LENGTH 50 VALUE 'Exp. Qtd.',

        TEXT_N009 TYPE C LENGTH 50 VALUE 'Memo Remetente',
        TEXT_N010 TYPE C LENGTH 50 VALUE 'Memo Número',
        TEXT_N011 TYPE C LENGTH 50 VALUE 'Memo DDE',
        TEXT_N012 TYPE C LENGTH 50 VALUE 'Memo RE',
        TEXT_N013 TYPE C LENGTH 50 VALUE 'Memo Qtd. Nota Prod.',

        TEXT_N014 TYPE C LENGTH 50 VALUE 'Prod. Doc. NF',
        TEXT_N015 TYPE C LENGTH 50 VALUE 'Prod. Número',
        TEXT_N016 TYPE C LENGTH 50 VALUE 'Prod. Série',
        TEXT_N017 TYPE C LENGTH 50 VALUE 'Prod. Qtd NF',
        TEXT_N018 TYPE C LENGTH 50 VALUE 'Prod. Qtd Vinculado',
        TEXT_N019 TYPE C LENGTH 50 VALUE 'Prod. Qtd Saldo',

        TEXT_N020 TYPE C LENGTH 50 VALUE 'Memorando'.

  IF PRIM_ALV IS INITIAL.

    CREATE OBJECT CONTAINER_ALV
      EXPORTING
        CONTAINER_NAME = 'CTN_ALV'.

    CREATE OBJECT ALV
      EXPORTING
        I_PARENT = CONTAINER_ALV.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_ALV USING:
        TABELA 'POSSUI_ZSTD0014'  TEXT_N001 ' ' 01 02 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'VGBEL'            TEXT_N002 ' ' 02 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'VBELN'            TEXT_N003 ' ' 03 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'EXP_DOCNUM'       TEXT_N004 ' ' 04 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'EXP_SERIE'        TEXT_N006 ' ' 05 04 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'EXP_NUMERO'       TEXT_N005 ' ' 06 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'EXP_DT_EMISSAO'   TEXT_N007 ' ' 07 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'EXP_QUANTIDADE'   TEXT_N008 ' ' 08 15 SPACE SPACE   'X'   SPACE SPACE SPACE SPACE,
        TABELA 'REMETENTE'        TEXT_N009 ' ' 09 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'NUMERO_MEMO'      TEXT_N010 ' ' 10 05 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'NR_DDE'           TEXT_N011 ' ' 11 10 SPACE 'ZDDEX' SPACE SPACE SPACE SPACE SPACE,
        TABELA 'NR_RE'            TEXT_N012 ' ' 12 10 SPACE 'ZREEX' SPACE SPACE SPACE SPACE SPACE,
        TABELA 'QTD_VINC_MEMO'    TEXT_N013 ' ' 13 15 SPACE SPACE   'X'   SPACE SPACE SPACE SPACE,
        TABELA 'DOCNUM'           TEXT_N014 ' ' 14 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'NFENUM'           TEXT_N015 ' ' 15 10 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'SERIES'           TEXT_N016 ' ' 16 04 SPACE SPACE   SPACE SPACE SPACE SPACE SPACE,
        TABELA 'QUANTIDADE'       TEXT_N017 ' ' 17 15 SPACE SPACE   'X'   SPACE SPACE SPACE SPACE,
        TABELA 'VINCULADO'        TEXT_N018 ' ' 18 15 SPACE SPACE   'X'   SPACE SPACE SPACE SPACE,
        TABELA 'SALDO'            TEXT_N019 ' ' 19 15 SPACE SPACE   'X'   SPACE SPACE SPACE SPACE,
        TABELA 'STATUS_MEMO'      TEXT_N020 ' ' 19 05 SPACE SPACE   SPACE 'X'   SPACE SPACE SPACE.

    CLEAR: GS_LAYOUT.
    GS_LAYOUT-ZEBRA    = 'X'.
    GS_LAYOUT-SEL_MODE = 'A'.

    CALL METHOD ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = GS_LAYOUT
      CHANGING
        IT_FIELDCATALOG = CATALOGO_ALV
        IT_OUTTAB       = IT_SAIDA[].

    PRIM_ALV = 'X'.

  ENDIF.

  CALL METHOD ALV->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = SCROLL_COL
      IS_ROW_NO   = SCROLL_ROW.

ENDFORM.                    " CRIA_ALV

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.

  DATA: VG_NOTA_D TYPE ZDOC_MEMO_NOTA_D.

  DATA: IT_SELECTED_ROWS   TYPE LVC_T_ROW,
        WA_SELECTED_ROWS   TYPE LVC_S_ROW,
        WA_NOTA_PROD       TYPE ZDOC_NF_PRODUTOR,
        IT_SAIDA_SEL       TYPE TABLE OF TP_REGISTRO_DET WITH HEADER LINE,
        VG_QUANTIDADE_MEMO TYPE ZDOC_MEMORANDO-QUANTIDADE_MEMO.

  CLEAR: IT_SELECTED_ROWS, IT_SAIDA_SEL[].

  CALL METHOD ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_SAIDA INDEX WA_SELECTED_ROWS-INDEX.
    MOVE-CORRESPONDING IT_SAIDA TO IT_SAIDA_SEL.
    APPEND IT_SAIDA_SEL.
  ENDLOOP.

  CASE OK_CODE.
    WHEN 'REMOVER'.
      "Somente permitir retirar se estiver com memornado aberto/pendente e saldo negativo de nota compromissada
      LOOP AT IT_SAIDA_SEL.
        IF ( IT_SAIDA_SEL-SALDO LT 0 ) AND
           ( IT_SAIDA_SEL-STATUS_MM NE 'T' ) AND
           ( IT_SAIDA_SEL-STATUS_MM NE 'F' ) AND
           ( NOT IT_SAIDA_SEL-NR_MEMORANDO IS INITIAL ) AND
           ( NOT IT_SAIDA_SEL-DOCNUM IS INITIAL ).
          READ TABLE IT_NF_MEMO WITH KEY NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO
                                         DOCNUM       = IT_SAIDA_SEL-DOCNUM
                                         ITMNUM       = IT_SAIDA_SEL-ITMNUM.
          IF SY-SUBRC IS INITIAL.
            MOVE-CORRESPONDING IT_NF_MEMO TO VG_NOTA_D.
            VG_NOTA_D-USUARIO	= SY-UNAME.
            VG_NOTA_D-DATA    = SY-DATUM.
            VG_NOTA_D-HORA    = SY-UZEIT.
            MODIFY ZDOC_MEMO_NOTA_D FROM VG_NOTA_D.
            DELETE FROM ZDOC_MEMO_NOTA WHERE NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO
                                         AND DOCNUM       = IT_SAIDA_SEL-DOCNUM
                                         AND ITMNUM       = IT_SAIDA_SEL-ITMNUM.
            UPDATE ZDOC_MEMORANDO
               SET STATUS = SPACE
             WHERE NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO.
          ENDIF.
        ENDIF.
      ENDLOOP.
    WHEN 'VINCULO'.
      "Somente permitir retirar se estiver com memornado protocolado/finalizado e saldo negativo de nota compromissada
      LOOP AT IT_SAIDA_SEL.
        IF ( IT_SAIDA_SEL-SALDO LT 0 ) AND
           ( ( IT_SAIDA_SEL-STATUS_MM EQ 'T' ) OR ( IT_SAIDA_SEL-STATUS_MM EQ 'F' ) ) AND
           ( NOT IT_SAIDA_SEL-NR_MEMORANDO IS INITIAL ) AND
           ( NOT IT_SAIDA_SEL-DOCNUM IS INITIAL ) AND
           ( IT_SAIDA_SEL-POSSUI_ZSTD0014 IS INITIAL ).
          IT_SAIDA_SEL-SALDO = IT_SAIDA_SEL-SALDO * -1.
          IF ( IT_SAIDA_SEL-SALDO LE IT_SAIDA_SEL-QUANTIDADE ).
            READ TABLE IT_NF_MEMO WITH KEY NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO
                                           DOCNUM       = IT_SAIDA_SEL-DOCNUM
                                           ITMNUM       = IT_SAIDA_SEL-ITMNUM.
            IF SY-SUBRC IS INITIAL.
              WA_NOTA_PROD-VBELN       = IT_SAIDA_SEL-VGBEL.
              WA_NOTA_PROD-DOCNUM_PROD = IT_NF_MEMO-DOCNUM.
              WA_NOTA_PROD-ITMNUM_PROD = IT_NF_MEMO-ITMNUM.
              WA_NOTA_PROD-MENGE       = IT_SAIDA_SEL-SALDO.
              MODIFY ZDOC_NF_PRODUTOR FROM WA_NOTA_PROD.
            ENDIF.
          ENDIF.
        ELSEIF IT_SAIDA_SEL-POSSUI_ZSTD0014 IS INITIAL.
          WA_NOTA_PROD-VBELN       = IT_SAIDA_SEL-VGBEL.
          WA_NOTA_PROD-DOCNUM_PROD = IT_SAIDA_SEL-DOCNUM.
          WA_NOTA_PROD-ITMNUM_PROD = IT_SAIDA_SEL-ITMNUM.
          WA_NOTA_PROD-MENGE       = IT_SAIDA_SEL-QTD_VINC_MEMO.
          MODIFY ZDOC_NF_PRODUTOR FROM WA_NOTA_PROD.
        ENDIF.
      ENDLOOP.
    WHEN 'AVINCULO'.
      "Somente permitir retirar se estiver com memornado aberto/pendente e saldo negativo de nota compromissada
      LOOP AT IT_SAIDA_SEL.
        IF ( IT_SAIDA_SEL-SALDO LT 0 ) AND
           ( IT_SAIDA_SEL-STATUS_MM NE 'T' ) AND
           ( IT_SAIDA_SEL-STATUS_MM NE 'F' ) AND
           ( NOT IT_SAIDA_SEL-NR_MEMORANDO IS INITIAL ) AND
           ( NOT IT_SAIDA_SEL-DOCNUM IS INITIAL ).
          READ TABLE IT_NF_MEMO WITH KEY NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO
                                         DOCNUM       = IT_SAIDA_SEL-DOCNUM
                                         ITMNUM       = IT_SAIDA_SEL-ITMNUM.
          IF SY-SUBRC IS INITIAL.

            IT_SAIDA_SEL-SALDO = IT_SAIDA_SEL-SALDO * -1.

            MOVE-CORRESPONDING IT_NF_MEMO TO VG_NOTA_D.
            VG_NOTA_D-MENGE   = IT_SAIDA_SEL-SALDO.
            VG_NOTA_D-USUARIO	= SY-UNAME.
            VG_NOTA_D-DATA    = SY-DATUM.
            VG_NOTA_D-HORA    = SY-UZEIT.
            MODIFY ZDOC_MEMO_NOTA_D FROM VG_NOTA_D.

            VG_NOTA_D-MENGE = IT_NF_MEMO-MENGE - IT_SAIDA_SEL-SALDO.

            IF VG_NOTA_D-MENGE GT 0.
              UPDATE ZDOC_MEMO_NOTA
                 SET MENGE = VG_NOTA_D-MENGE
               WHERE NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO
                 AND DOCNUM       = IT_SAIDA_SEL-DOCNUM
                 AND ITMNUM       = IT_SAIDA_SEL-ITMNUM.

              UPDATE ZDOC_NF_PRODUTOR
                 SET MENGE = VG_NOTA_D-MENGE
               WHERE VBELN       = IT_SAIDA_SEL-VGBEL
                 AND DOCNUM_PROD = IT_NF_MEMO-DOCNUM
                 AND ITMNUM_PROD = IT_NF_MEMO-ITMNUM.
            ELSE.
              DELETE FROM ZDOC_MEMO_NOTA
               WHERE NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO
                 AND DOCNUM       = IT_SAIDA_SEL-DOCNUM
                 AND ITMNUM       = IT_SAIDA_SEL-ITMNUM.
              DELETE FROM ZDOC_NF_PRODUTOR
               WHERE VBELN        = IT_SAIDA_SEL-VGBEL
                 AND DOCNUM_PROD  = IT_NF_MEMO-DOCNUM
                 AND ITMNUM_PROD  = IT_NF_MEMO-ITMNUM.
            ENDIF.

            VG_QUANTIDADE_MEMO = 0.

            SELECT SUM( MENGE ) INTO VG_QUANTIDADE_MEMO
              FROM ZDOC_MEMO_NOTA
             WHERE NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO.

            UPDATE ZDOC_MEMORANDO
               SET QUANTIDADE_MEMO = VG_QUANTIDADE_MEMO
             WHERE NR_MEMORANDO = IT_SAIDA_SEL-NR_MEMORANDO.

          ENDIF.
        ENDIF.
      ENDLOOP.
  ENDCASE.

  PERFORM PESQUISAR.

ENDMODULE.                 " USER_COMMAND_0001  INPUT



*&---------------------------------------------------------------------*
*&      Form  PESQUISAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR .

  TYPES: BEGIN OF TP_REF.
  TYPES:   DOCNUM TYPE J_1BDOCNUM,
           REFKEY	TYPE J_1BREFKEY,
           VBELN  TYPE VBELN_VF,
           VGBEL  TYPE VGBEL,
           AUBEL  TYPE VBELN_VA.
  TYPES: END OF TP_REF.

  DATA: IT_NOTAS         TYPE TABLE OF ZEXPORT_NOTAS     WITH HEADER LINE,
        IT_NOTAS_AUX     TYPE TABLE OF ZEXPORT_NOTAS     WITH HEADER LINE,
        IT_NF_MEMO_AUX   TYPE TABLE OF ZDOC_MEMO_NOTA    WITH HEADER LINE,
        IT_MEMORANDO     TYPE TABLE OF ZDOC_MEMORANDO    WITH HEADER LINE,
        IT_MEMORANDO_AUX TYPE TABLE OF ZDOC_MEMORANDO    WITH HEADER LINE,
        IT_NF_EXP        TYPE TABLE OF ZDOC_MEMO_NF_EXP  WITH HEADER LINE,
        IT_NF_EXP_AUX    TYPE TABLE OF ZDOC_MEMO_NF_EXP  WITH HEADER LINE,
        IT_J_1BNFLIN     TYPE TABLE OF J_1BNFLIN         WITH HEADER LINE,
        IT_DOC_FATURA    TYPE TABLE OF TP_REF            WITH HEADER LINE,
        IT_VBRP          TYPE TABLE OF VBRP              WITH HEADER LINE,
        IT_VBRP_AUX      TYPE TABLE OF VBRP              WITH HEADER LINE,
        IT_NF_PRODUTOR   TYPE TABLE OF ZDOC_NF_PRODUTOR  WITH HEADER LINE.

  TRY .
      CALL FUNCTION 'Z_EXPORT_TERCEIRO_ACOMP'
        EXPORTING
          P_BUKRS     = P_EMPRES
          P_WERKS     = P_CENTRO
          P_MATNR     = P_MATER
          P_REMETENTE = P_REMET
          P_DIRECAO   = '1'
        TABLES
          IT_NOTAS    = IT_NOTAS
        EXCEPTIONS
          CFOPS_SAIDA = 1
          OTHERS      = 2.
    CATCH CX_ROOT INTO LR_EXC.
      LV_MESSAGE = LR_EXC->GET_TEXT( ).
      MESSAGE E836(SD) WITH 'Erro ao buscar notas fiscais.' LV_MESSAGE.
  ENDTRY.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR: IT_NOTAS_AUX[], IT_NF_MEMO[], IT_SAIDA[].
  MOVE: IT_NOTAS[] TO IT_NOTAS_AUX[].
  SORT IT_NOTAS_AUX BY DOCNUM.
  DELETE ADJACENT DUPLICATES FROM IT_NOTAS_AUX COMPARING DOCNUM.

  IF NOT IT_NOTAS_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_NF_MEMO
      FROM ZDOC_MEMO_NOTA
       FOR ALL ENTRIES IN IT_NOTAS_AUX
     WHERE DOCNUM EQ IT_NOTAS_AUX-DOCNUM.
  ELSE.
    MESSAGE 'Empresa/Filial/Material não possui compromisso!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF IT_NF_MEMO[] IS INITIAL.
    MESSAGE 'Empresa/Filial/Material não possui comprovação!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: IT_NF_MEMO_AUX[].
  MOVE:  IT_NF_MEMO[] TO IT_NF_MEMO_AUX[].
  SORT   IT_NF_MEMO_AUX BY NR_MEMORANDO.
  DELETE ADJACENT DUPLICATES FROM IT_NF_MEMO_AUX COMPARING NR_MEMORANDO.

  IF NOT IT_NF_MEMO_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_MEMORANDO
      FROM ZDOC_MEMORANDO
       FOR ALL ENTRIES IN IT_NF_MEMO_AUX
     WHERE NR_MEMORANDO EQ IT_NF_MEMO_AUX-NR_MEMORANDO.
*       AND remetente IN p_remet.
  ENDIF.

  CLEAR: IT_MEMORANDO_AUX[].
  MOVE:  IT_MEMORANDO[] TO IT_MEMORANDO_AUX[].
  SORT   IT_MEMORANDO_AUX BY NR_NOTA_EXP.
  DELETE ADJACENT DUPLICATES FROM IT_MEMORANDO_AUX COMPARING NR_NOTA_EXP.

  IF NOT IT_MEMORANDO_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_NF_EXP
      FROM ZDOC_MEMO_NF_EXP
       FOR ALL ENTRIES IN IT_MEMORANDO_AUX
     WHERE NR_NOTA_EXP EQ IT_MEMORANDO_AUX-NR_NOTA_EXP
       AND DT_EMISSAO_NOTA IN P_DTNOTA.
  ENDIF.

  CLEAR: IT_NF_EXP_AUX[].
  MOVE:  IT_NF_EXP[] TO IT_NF_EXP_AUX[].
  SORT   IT_NF_EXP_AUX BY DOCNUM.
  DELETE ADJACENT DUPLICATES FROM IT_NF_EXP_AUX COMPARING DOCNUM.

  IF NOT IT_NF_EXP_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_J_1BNFLIN
      FROM J_1BNFLIN
       FOR ALL ENTRIES IN IT_NF_EXP_AUX
     WHERE REFTYP EQ 'BI'
       AND DOCNUM EQ IT_NF_EXP_AUX-DOCNUM.
  ENDIF.

  LOOP AT IT_J_1BNFLIN.
    CLEAR: IT_DOC_FATURA.
    IT_DOC_FATURA-DOCNUM = IT_J_1BNFLIN-DOCNUM.
    IT_DOC_FATURA-REFKEY = IT_J_1BNFLIN-REFKEY.
    IT_DOC_FATURA-VBELN  = IT_J_1BNFLIN-REFKEY(10).
    APPEND IT_DOC_FATURA.
  ENDLOOP.

  SELECT * INTO TABLE IT_VBRP
    FROM VBRP
     FOR ALL ENTRIES IN IT_DOC_FATURA
   WHERE VBELN EQ IT_DOC_FATURA-VBELN.

  LOOP AT IT_VBRP.
    READ TABLE IT_DOC_FATURA WITH KEY VBELN = IT_VBRP-VBELN.
    IF SY-SUBRC IS INITIAL.
      IT_DOC_FATURA-VGBEL = IT_VBRP-VGBEL.
      IT_DOC_FATURA-AUBEL = IT_VBRP-AUBEL.
      MODIFY IT_DOC_FATURA INDEX SY-TABIX TRANSPORTING VGBEL AUBEL.
    ENDIF.
  ENDLOOP.

  CLEAR: IT_VBRP_AUX[].
  MOVE IT_VBRP[] TO IT_VBRP_AUX[].
  SORT IT_VBRP_AUX BY VGBEL.
  DELETE ADJACENT DUPLICATES FROM IT_VBRP_AUX COMPARING VGBEL.

  IF NOT IT_VBRP_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_NF_PRODUTOR
      FROM ZDOC_NF_PRODUTOR
       FOR ALL ENTRIES IN IT_VBRP_AUX
     WHERE VBELN EQ IT_VBRP_AUX-VGBEL.
  ENDIF.

  LOOP AT IT_NF_MEMO.

    CLEAR: IT_SAIDA.

    IT_SAIDA-NR_MEMORANDO  = IT_NF_MEMO-NR_MEMORANDO.
    IT_SAIDA-QTD_VINC_MEMO = IT_NF_MEMO-MENGE.

    READ TABLE IT_MEMORANDO WITH KEY NR_MEMORANDO = IT_NF_MEMO-NR_MEMORANDO.
    IF SY-SUBRC IS INITIAL.
      IT_SAIDA-REMETENTE   = IT_MEMORANDO-REMETENTE.
      IT_SAIDA-NUMERO_MEMO = IT_MEMORANDO-NUMERO_MEMO.
      IT_SAIDA-NR_DDE      = IT_MEMORANDO-NR_DDE.
      IT_SAIDA-DT_DDE      = IT_MEMORANDO-DT_DDE.
      IT_SAIDA-NR_RE       = IT_MEMORANDO-NR_RE.
      IT_SAIDA-DT_RE       = IT_MEMORANDO-DT_RE.
      IT_SAIDA-STATUS_MM   = IT_MEMORANDO-STATUS.

      IF IT_MEMORANDO-CANCELADO IS NOT INITIAL.
        IT_SAIDA-STATUS_MEMO = ICON_CANCEL.
      ELSEIF IT_MEMORANDO-STATUS IS INITIAL.
        IT_SAIDA-STATUS_MEMO = ICON_LED_RED.
      ELSEIF IT_MEMORANDO-STATUS EQ 'C'.
        IT_SAIDA-STATUS_MEMO = ICON_LED_INACTIVE.
      ELSEIF IT_MEMORANDO-STATUS EQ 'F'.
        IT_SAIDA-STATUS_MEMO = ICON_COMPLETE.
      ELSEIF IT_MEMORANDO-STATUS EQ 'T'.
        IT_SAIDA-STATUS_MEMO = ICON_INCOMPLETION_LOG.
      ELSEIF IT_MEMORANDO-STATUS EQ 'P'.
        IT_SAIDA-STATUS_MEMO = ICON_FAILURE.
      ELSE.
        IT_SAIDA-STATUS_MEMO = ICON_LED_YELLOW.
      ENDIF.

      READ TABLE IT_NF_EXP WITH KEY NR_NOTA_EXP = IT_MEMORANDO-NR_NOTA_EXP.
      IF SY-SUBRC IS INITIAL.

        IT_SAIDA-EXP_DOCNUM     = IT_NF_EXP-DOCNUM.
        IT_SAIDA-EXP_SERIE      = IT_NF_EXP-SERIE.
        IT_SAIDA-EXP_NUMERO     = IT_NF_EXP-NUMERO_NOTA.
        IT_SAIDA-EXP_DT_EMISSAO = IT_NF_EXP-DT_EMISSAO_NOTA.
        IT_SAIDA-EXP_QUANTIDADE = IT_NF_EXP-QUANTIDADE.

        READ TABLE IT_DOC_FATURA WITH KEY DOCNUM = IT_NF_EXP-DOCNUM.
        IF SY-SUBRC IS INITIAL.
          IT_SAIDA-VBELN = IT_DOC_FATURA-VBELN.
          IT_SAIDA-VGBEL = IT_DOC_FATURA-VGBEL.
          IT_SAIDA-AUBEL = IT_DOC_FATURA-AUBEL.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    LOOP AT IT_NOTAS WHERE DOCNUM EQ IT_NF_MEMO-DOCNUM.
      MOVE-CORRESPONDING IT_NOTAS TO IT_SAIDA.

      IF NOT IT_SAIDA-VGBEL IS INITIAL.
        READ TABLE IT_NF_PRODUTOR WITH KEY DOCNUM_PROD = IT_NOTAS-DOCNUM
                                           VBELN       = IT_SAIDA-VGBEL.
        IF SY-SUBRC IS INITIAL.
          IT_SAIDA-POSSUI_ZSTD0014 = 'X'.
        ENDIF.
      ENDIF.

      APPEND IT_SAIDA.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " PESQUISAR
