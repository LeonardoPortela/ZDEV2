*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDR0001                                                *
* Descrição  : Impressão NF's Vinculadas                               *
* Módulo     : SD                                Transação:            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 10/08/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT ZSDR0001 NO STANDARD PAGE HEADING MESSAGE-ID SD.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TY_J_1BNFLIN,
    DOCNUM TYPE J_1BNFLIN-DOCNUM,
    REFKEY TYPE J_1BNFLIN-REFKEY,
    VBELN  TYPE VBFA-VBELN,
    NBM    TYPE J_1BNFLIN-NBM,
  END OF TY_J_1BNFLIN,

  BEGIN OF TY_VBFA,
    VBELN TYPE VBFA-VBELN,
    VBELV TYPE VBFA-VBELV,
  END OF TY_VBFA,

  BEGIN OF TY_ZSDT_EXPORT,
    ORDEM  TYPE ZSDT_EXPORT-ORDEM,
    DOCNUM TYPE ZSDT_EXPORT-DOCNUM,
  END OF TY_ZSDT_EXPORT,

  BEGIN OF TY_J_1BNFDOC,
    DOCNUM TYPE J_1BNFDOC-DOCNUM,
    DOCDAT TYPE J_1BNFDOC-DOCDAT,
    NFENUM TYPE J_1BNFDOC-NFENUM,
  END OF TY_J_1BNFDOC,

  BEGIN OF TY_SAIDA_EXCEL,
    NFENUM     TYPE ZSDT_RETLOTE-NFENUM,
    DATA_SAIDA TYPE ZSDT_RETLOTE-DATA_SAIDA,
    VCHAVE     TYPE ZSDTCHAVE-VCHAVE,
    NBM        TYPE J_1BNFLIN-NBM,
    QUANT_VINC TYPE ZSDT_RETLOTE-QUANT_VINC,
    VLR_TOTAL  TYPE ZSDT_RETLOTE-VLR_TOTAL,
    UNITARIO   TYPE ZSDT_RETLOTE-UNITARIO,
  END OF TY_SAIDA_EXCEL,

  BEGIN OF TY_ZSDT_RETLOTE.
        INCLUDE STRUCTURE ZSDT_RETLOTE.
TYPES:  NBM TYPE J_1BNFLIN-NBM,
        END OF  TY_ZSDT_RETLOTE.


*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: "T_RETLOTE     TYPE TABLE OF ZSDT_RETLOTE,
  T_RETLOTE     TYPE TABLE OF ZSDF0001,
  T_J_1BNFDOC   TYPE TABLE OF TY_J_1BNFDOC,
  T_J_1BNFLIN   TYPE TABLE OF TY_J_1BNFLIN,
  T_VBFA        TYPE TABLE OF TY_VBFA,
  T_ZSDT_EXPORT TYPE TABLE OF TY_ZSDT_EXPORT,
  T_RETCHAVE    TYPE TABLE OF ZSDTCHAVE,
  T_SAIDA_EXCEL TYPE TABLE OF TY_SAIDA_EXCEL.

DATA: W_J_1BNFDOC   TYPE TY_J_1BNFDOC,
      W_J_1BNFLIN   TYPE TY_J_1BNFLIN,
      W_VBFA        TYPE TY_VBFA,
      W_ZSDT_EXPORT TYPE TY_ZSDT_EXPORT,
      W_RETCHAVE    TYPE ZSDTCHAVE,
      W_SAIDA_EXCEL TYPE  TY_SAIDA_EXCEL.


DATA: BEGIN OF T_SAIDA OCCURS 0,
        DESC01 TYPE STRING,
        DESC02 TYPE STRING,
        DESC03 TYPE STRING,
        DESC04 TYPE STRING,
        DESC05 TYPE STRING,
        DESC06 TYPE STRING,
        DESC07 TYPE STRING,
        DESC08 TYPE STRING,
      END OF T_SAIDA.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: V_BUTXT  TYPE T001-BUTXT,
      V_NAME1  TYPE T001W-NAME1,
      V_LGOBE  TYPE T001L-LGOBE,
      V_MAKTX  TYPE MAKT-MAKTX,
      V_NAME   TYPE KNA1-NAME1,
      V_STREET TYPE ADRC-STREET,
      V_NUM1   TYPE ADRC-HOUSE_NUM1.

DATA: "SL_RETLOTE TYPE ZSDT_RETLOTE,
  SL_RETLOTE TYPE ZSDF0001,
  VL_ADRNR   TYPE LFA1-ADRNR.

DATA: LW_CHAVE  TYPE C LENGTH 44.


DATA: MAIN_TEXT      TYPE BCSY_TEXT,
      BINARY_CONTENT TYPE SOLIX_TAB,
      SIZE           TYPE SO_OBJ_LEN,
      SENT_TO_ALL    TYPE OS_BOOLEAN.


DATA: VL_CALL_MASK(50) TYPE C,
      VL_NAME_EXP      LIKE IBIPPARMS-PATH,
      VL_FILENAME_EXP  TYPE STRING.

DATA: GS_EXCEL    TYPE OLE2_OBJECT,       " Objeto Excel
      GS_WORKBOOK TYPE OLE2_OBJECT,       " Workbook 'Area de trabalho'
      GS_SHEET    TYPE OLE2_OBJECT,       " Planilha
      GS_CELL1    TYPE OLE2_OBJECT,                     " Celula 1
      GS_CELL2    TYPE OLE2_OBJECT,                     " Celula 2
      GS_CELLS    TYPE OLE2_OBJECT,       " Células
      GS_RANGE    TYPE OLE2_OBJECT,       " Grupo de células
      GS_FONT     TYPE OLE2_OBJECT,       " Fonte da célula
      GS_COLUMN   TYPE OLE2_OBJECT.       " Coluna da célula

DATA : LINHA     TYPE I,      " Atribui Valor Linha
       COLUNA    TYPE I,
       V_TEXTO   TYPE STRING,            " Conteudo das celulas
       VDATA(10) TYPE C.


*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA S_RETLOTE TYPE ZSDT_RETLOTE.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK A2 WITH FRAME.
PARAMETERS:
  P_DOCNUM TYPE J_1BNFDOC-DOCNUM,
  P_DOCEXP TYPE J_1BNFDOC-DOCNUM.

SELECTION-SCREEN SKIP 1.

PARAMETERS: GEXCEL AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK A2.
SELECTION-SCREEN END   OF BLOCK A1.



AT SELECTION-SCREEN OUTPUT.

  IF P_DOCNUM IS INITIAL AND P_DOCEXP IS INITIAL.
    MESSAGE 'Informe um documento' TYPE 'I'.
    SET CURSOR FIELD 'P_DOCNUM' .
  ENDIF.

  IF P_DOCNUM IS NOT INITIAL AND P_DOCEXP IS NOT INITIAL.
    MESSAGE 'Informe apenas um documento' TYPE 'I'.
    SET CURSOR FIELD 'P_DOCNUM' .
  ENDIF.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.


* Seleção Dados
  PERFORM: Z_SELECIONA_DADOS.

  IF GEXCEL = 'X'.
    PERFORM Z_MONTA_EXCEL.
  ELSE.
* Chama Formulário
    PERFORM  Z_CALL_FORM.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                             Seleção Dados                            *
*----------------------------------------------------------------------*
FORM Z_SELECIONA_DADOS.

* Seleciona Tabela ZSDT_RETLOTE
  PERFORM: Z_SELECIONA_RETLOTE,

* Seleciona Dados Cabeçalho
           Z_SELECIONA_CABEC.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_RETLOTE                                      *
*&---------------------------------------------------------------------*
*                    Seleciona Tabela ZSDT_RETLOTE                     *
*----------------------------------------------------------------------*
FORM Z_SELECIONA_RETLOTE.
  IF P_DOCNUM IS NOT INITIAL.
    SELECT R~DOCNUM  R~NFENUM   R~WERKS  R~NF_RETORNO  R~DOCNUM_RET
           R~DATA_CRIACAO  R~DATA_SAIDA  R~SAFRA   R~LGORT  R~PARCEIRO
           R~MATNR  R~BUKRS  R~QUANT_VINC   R~VLR_TOTAL  R~UNITARIO
           R~STATUS  R~VBELN  R~INSERT_BD  R~DT_INSERT_BD R~HR_INSERT_BD
           J~NBM
      FROM ZSDT_RETLOTE AS R
      INNER JOIN  J_1BNFLIN AS J ON J~DOCNUM = R~DOCNUM
      INTO TABLE T_RETLOTE
    WHERE  R~DOCNUM_RET EQ P_DOCNUM
      AND  R~STATUS     EQ 'V'.

    IF NOT SY-SUBRC IS INITIAL.
      MESSAGE I836 WITH TEXT-002.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    SELECT SINGLE DOCNUM DOCDAT NFENUM
      FROM J_1BNFDOC
      INTO W_J_1BNFDOC
      WHERE DOCNUM = P_DOCEXP.
    IF SY-SUBRC EQ 0.
      SELECT DOCNUM REFKEY NBM
      FROM J_1BNFLIN
      INTO TABLE T_J_1BNFLIN
      WHERE DOCNUM = W_J_1BNFDOC-DOCNUM.
      LOOP AT T_J_1BNFLIN INTO W_J_1BNFLIN.
        W_J_1BNFLIN-VBELN = W_J_1BNFLIN-REFKEY.
        MODIFY T_J_1BNFLIN FROM W_J_1BNFLIN INDEX SY-TABIX TRANSPORTING VBELN.
      ENDLOOP.
      IF SY-SUBRC = 0.
        SELECT VBELN VBELV
        FROM VBFA
        INTO TABLE T_VBFA
        FOR ALL ENTRIES IN T_J_1BNFLIN
        WHERE VBELN   = T_J_1BNFLIN-VBELN
        AND   VBTYP_V = 'C'.
        IF SY-SUBRC = 0.
          SELECT ORDEM DOCNUM
            FROM ZSDT_EXPORT
            INTO TABLE T_ZSDT_EXPORT
            FOR ALL ENTRIES IN T_VBFA
            WHERE ORDEM = T_VBFA-VBELV.
          IF SY-SUBRC = 0.
            READ TABLE T_ZSDT_EXPORT INTO W_ZSDT_EXPORT INDEX 1.

            SELECT  R~DOCNUM  R~NFENUM   R~WERKS  R~NF_RETORNO  R~DOCNUM_RET
                   R~DATA_CRIACAO  R~DATA_SAIDA  R~SAFRA   R~LGORT  R~PARCEIRO
                   R~MATNR  R~BUKRS  R~QUANT_VINC   R~VLR_TOTAL  R~UNITARIO
                   R~STATUS  R~VBELN  R~INSERT_BD  R~DT_INSERT_BD R~HR_INSERT_BD
                   J~NBM
              FROM ZSDT_RETLOTE AS R
              INNER JOIN  J_1BNFLIN AS J ON J~DOCNUM = R~DOCNUM
              INTO TABLE T_RETLOTE
            WHERE R~DOCNUM_RET EQ W_ZSDT_EXPORT
            AND  R~STATUS     EQ 'V'.

            IF NOT SY-SUBRC IS INITIAL.
              MESSAGE I836 WITH TEXT-002.
              LEAVE LIST-PROCESSING.
            ENDIF.

            LOOP AT T_RETLOTE INTO SL_RETLOTE.
              SL_RETLOTE-DATA_CRIACAO = W_J_1BNFDOC-DOCDAT.
              SL_RETLOTE-NF_RETORNO   = W_J_1BNFDOC-NFENUM.
              MODIFY T_RETLOTE FROM SL_RETLOTE INDEX SY-TABIX TRANSPORTING DATA_CRIACAO NF_RETORNO.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.


ENDFORM.                    " Z_SELECIONA_RETLOTE

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_CABEC                                        *
*&---------------------------------------------------------------------*
*                       Seleciona Dados Cabeçalho                      *
*----------------------------------------------------------------------*
FORM Z_SELECIONA_CABEC.


  READ TABLE T_RETLOTE INTO SL_RETLOTE INDEX 1.

  SELECT SINGLE BUTXT
    FROM T001
    INTO V_BUTXT
  WHERE  BUKRS EQ SL_RETLOTE-BUKRS.

  SELECT SINGLE NAME1
    FROM T001W
    INTO V_NAME1
  WHERE  WERKS EQ SL_RETLOTE-WERKS.

  SELECT SINGLE LGOBE
    FROM T001L
    INTO V_LGOBE
  WHERE  LGORT EQ SL_RETLOTE-LGORT.

  SELECT SINGLE MAKTX
    FROM MAKT
    INTO V_MAKTX
  WHERE  SPRAS EQ 'PT'
    AND  MATNR EQ SL_RETLOTE-MATNR.

  SELECT SINGLE NAME1 ADRNR
    FROM  LFA1
    INTO (V_NAME, VL_ADRNR)
  WHERE  LIFNR EQ SL_RETLOTE-PARCEIRO.

  SELECT SINGLE STREET HOUSE_NUM1
    FROM ADRC
    INTO (V_STREET, V_NUM1)
  WHERE  ADDRNUMBER EQ VL_ADRNR.


  LOOP AT T_RETLOTE INTO DATA(W_RETLOTE).

    CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
      EXPORTING
        I_DOCNUM = W_RETLOTE-DOCNUM
        I_VALIDA = 'X'
      RECEIVING
        E_CHAVE  = LW_CHAVE.

    W_RETCHAVE-DOCNUM = W_RETLOTE-DOCNUM.
    W_RETCHAVE-VCHAVE = LW_CHAVE.

    APPEND W_RETCHAVE TO T_RETCHAVE.
    CLEAR: W_RETCHAVE, W_RETLOTE, LW_CHAVE.

  ENDLOOP.

  SORT T_RETLOTE BY NFENUM ASCENDING.

ENDFORM.                    " Z_SELECIONA_CABEC

*&---------------------------------------------------------------------*
*&      Form  Z_CALL_FORM                                              *
*&---------------------------------------------------------------------*
*                            Chama Formulário                          *
*----------------------------------------------------------------------*
FORM Z_CALL_FORM.

  DATA: VL_FORMNAME TYPE TDSFNAME,
        VL_NAME     TYPE RS38L_FNAM.

  VL_FORMNAME = 'ZSDF0001'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = VL_FORMNAME
    IMPORTING
      FM_NAME            = VL_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CALL FUNCTION VL_NAME
    EXPORTING
      P_BUTXT          = V_BUTXT
      P_NAME1          = V_NAME1
      P_NAME           = V_NAME
      P_STREET         = V_STREET
      P_LGOBE          = V_LGOBE
      P_MAKTX          = V_MAKTX
      P_NUM1           = V_NUM1
    TABLES
      T_RETLOTE        = T_RETLOTE
      T_RETCHAVE       = T_RETCHAVE
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      USER_CANCELED    = 4
      OTHERS           = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " Z_CALL_FORM


FORM Z_MONTA_EXCEL.
  DATA: VDESC(20)    TYPE C,
        VSOMA(10)    TYPE C,
        VTOTALNF(50) TYPE C,
        VQUANT       TYPE ZSDT_RETLOTE-QUANT_VINC,
        VTOTAL       TYPE ZSDT_RETLOTE-VLR_TOTAL.

  PERFORM Z_GERA_EXCEL.

  PERFORM FORMATA_LARGURA USING 'E.E' '40'.

  READ TABLE T_RETLOTE INTO SL_RETLOTE INDEX 1.
  IF SY-SUBRC = 0.
    LINHA = LINHA + 1.

    V_TEXTO =  'Empresa:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    CONCATENATE SL_RETLOTE-BUKRS   '-' V_BUTXT INTO V_TEXTO.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    V_TEXTO =  'Filial:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    CONCATENATE SL_RETLOTE-WERKS   '-' V_NAME1 INTO V_TEXTO .
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    V_TEXTO =  'Safra:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    V_TEXTO  = SL_RETLOTE-SAFRA.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    V_TEXTO =  'Deposito:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    CONCATENATE SL_RETLOTE-LGORT '-' V_LGOBE INTO  V_TEXTO.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    V_TEXTO =  'Terminal:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    CONCATENATE SL_RETLOTE-PARCEIRO '-' V_NAME INTO  V_TEXTO.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    V_TEXTO =  'Endereço:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    V_TEXTO =  V_STREET.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.


    V_TEXTO = 'Material:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    CONCATENATE SL_RETLOTE-MATNR  '-' V_MAKTX INTO  V_TEXTO.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    V_TEXTO = 'Data Emissão'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    CONCATENATE  SL_RETLOTE-DATA_CRIACAO+6(2) '.'   SL_RETLOTE-DATA_CRIACAO+4(2) '.'  SL_RETLOTE-DATA_CRIACAO+0(4) INTO V_TEXTO.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    V_TEXTO =  'NF Retorno:'.
    PERFORM PREENCHE_DADOS USING LINHA 1 LINHA 5 V_TEXTO ' '.
    V_TEXTO = SL_RETLOTE-NF_RETORNO.
    PERFORM PREENCHE_DADOS USING LINHA 2 LINHA 5 V_TEXTO ' '.
    LINHA = LINHA + 1.

    LINHA  = LINHA  + 2.

    COLUNA = COLUNA + 1.
    V_TEXTO = 'Nota Fiscal'.
    PERFORM PREENCHE_DADOS USING LINHA COLUNA LINHA COLUNA  V_TEXTO ' '.

    COLUNA = COLUNA + 1.
    V_TEXTO = 'Chave'.
    PERFORM PREENCHE_DADOS USING LINHA COLUNA LINHA COLUNA  V_TEXTO ' '.

    COLUNA = COLUNA + 1.
    V_TEXTO = 'NCM'.
    PERFORM PREENCHE_DADOS USING LINHA COLUNA LINHA COLUNA  V_TEXTO ' '.

    COLUNA = COLUNA + 1.
    V_TEXTO = 'Dta Emissão'.
    PERFORM PREENCHE_DADOS USING LINHA COLUNA LINHA COLUNA  V_TEXTO ' '.

    COLUNA = COLUNA + 1.
    V_TEXTO = 'Quantidade'.
    PERFORM PREENCHE_DADOS USING LINHA COLUNA LINHA COLUNA  V_TEXTO ' '.

    COLUNA = COLUNA + 1.
    V_TEXTO = 'Unitário'.
    PERFORM PREENCHE_DADOS USING LINHA COLUNA LINHA COLUNA  V_TEXTO ' '.

    COLUNA = COLUNA + 1.
    V_TEXTO = 'Valor Total'.
    PERFORM PREENCHE_DADOS USING LINHA COLUNA LINHA COLUNA  V_TEXTO ' '.


    LOOP AT T_RETLOTE INTO SL_RETLOTE.

      LINHA = LINHA + 1.

      PERFORM PREENCHE_DETALHE USING LINHA 1 0 0 10 10 SL_RETLOTE-NFENUM ' '.
      VSOMA = VSOMA + 1.

      READ TABLE T_RETCHAVE INTO  W_RETCHAVE WITH KEY DOCNUM = SL_RETLOTE-DOCNUM.
      IF SY-SUBRC = 0.
        PERFORM PREENCHE_DETALHE USING LINHA 2 0 0 10 3 W_RETCHAVE-VCHAVE ' '.
      ENDIF.

      PERFORM PREENCHE_DETALHE USING LINHA 3 0 0 10 3 SL_RETLOTE-NBM ' '.
      CONCATENATE SL_RETLOTE-DATA_SAIDA+6(2) '.' SL_RETLOTE-DATA_SAIDA+4(2) '.' SL_RETLOTE-DATA_SAIDA+0(4) INTO VDATA.

      PERFORM PREENCHE_DETALHE USING LINHA 4 0 0 10 10 VDATA ' '.
      PERFORM PREENCHE_DETALHE USING LINHA 5 0 0 10 10 SL_RETLOTE-QUANT_VINC 'QUANTIDADE'.
      PERFORM PREENCHE_DETALHE USING LINHA 6 0 0 10 10 SL_RETLOTE-UNITARIO   'UNITARIO'.
      PERFORM PREENCHE_DETALHE USING LINHA 7 0 0 10 10 SL_RETLOTE-VLR_TOTAL  'VALOR'.

      VQUANT =  VQUANT  + SL_RETLOTE-QUANT_VINC.
      VTOTAL =  VTOTAL  + SL_RETLOTE-VLR_TOTAL.
    ENDLOOP.

    CONDENSE VSOMA.
    VTOTALNF = VSOMA.

    CONCATENATE 'Total NFs: ' VTOTALNF INTO VDESC.

    LINHA = LINHA + 1.
    PERFORM PREENCHE_DETALHE USING LINHA 1 2 0 10 10 VDESC ' '.
    PERFORM PREENCHE_DETALHE USING LINHA 4 2 0 10 10 'Total: ' ' '.
    PERFORM PREENCHE_DETALHE USING LINHA 6 2 0 10 10 SL_RETLOTE-UNITARIO 'UNITARIO'.
    PERFORM PREENCHE_DETALHE USING LINHA 5 2 0 10 10 VQUANT 'QUANTIDADE'.
    PERFORM PREENCHE_DETALHE USING LINHA 7 2 0 10 10 VTOTAL 'VALOR'.
  ENDIF.

  SET PROPERTY OF GS_EXCEL 'ScreenUpdating'   = 1.
  " Libera os cara aee.
  FREE OBJECT GS_SHEET.
  FREE OBJECT GS_WORKBOOK.
  FREE OBJECT GS_EXCEL.

ENDFORM.

FORM FORMATA_LARGURA  USING COLUNA LARGURA.

  CALL METHOD OF GS_EXCEL 'Range' = GS_RANGE
  EXPORTING
  #1 = COLUNA.

  SET PROPERTY OF GS_RANGE 'ColumnWidth' = LARGURA.
ENDFORM.


FORM Z_GERA_EXCEL.

  CREATE OBJECT GS_EXCEL 'Excel.Application'.

  SET PROPERTY OF GS_EXCEL 'ScreenUpdating' = 0.

  SET PROPERTY OF GS_EXCEL 'Visible'  = 1.

  GET PROPERTY OF GS_EXCEL 'Workbooks' = GS_WORKBOOK.

  CALL METHOD OF GS_WORKBOOK 'Add'.

  IF SY-SUBRC <> 0.
    MESSAGE 'Erro na abertura do Excel:' TYPE 'E'.
    STOP.
  ENDIF.

  CALL METHOD OF GS_EXCEL 'Worksheets' = GS_SHEET.

  CALL METHOD OF GS_SHEET 'Add' = GS_SHEET.

  SET PROPERTY OF GS_SHEET 'Name' = 'Relatório'.

  IF SY-SUBRC <> 0.
    MESSAGE 'Erro na abertura do Excel:' TYPE 'E'.
    STOP.
  ENDIF.
ENDFORM.

FORM PREENCHE_DADOS USING LINHA1 COLUNA1 LINHA2 COLUNA2 TEXTO CAMPO1.

  CALL METHOD OF GS_EXCEL 'Cells' = GS_CELL1
   EXPORTING
     #1 = LINHA1
     #2 = COLUNA1.

  CALL METHOD OF GS_EXCEL 'Cells' = GS_CELL2
   EXPORTING
     #1 = LINHA2
     #2 = COLUNA2.

  PERFORM PREENCHE_CELULA USING GS_CELL1 0 0 0 10 TEXTO CAMPO1.
ENDFORM.

FORM PREENCHE_CELULA USING CELULA
                           BOLD
                           ALINHAMENTO
                           LARGURA
                           TAM
                           VALOR
                           CAMPO.

  GET PROPERTY OF CELULA  'Font' = GS_FONT.
  SET PROPERTY OF GS_FONT 'Bold' = BOLD.
  SET PROPERTY OF GS_FONT 'Size' = TAM.
  SET PROPERTY OF CELULA  'HorizontalAlignment' =  ALINHAMENTO.

  CASE CAMPO.
    WHEN  'QUANTIDADE'.
      SET PROPERTY OF CELULA  'NumberFormat' = '#,###0.000'.
    WHEN 'UNITARIO'.
      SET PROPERTY OF CELULA  'NumberFormat' = '#,##0.00'.
    WHEN 'VALOR'.
      SET PROPERTY OF CELULA  'NumberFormat' = '#,###0.00'.   "'#,###0.000'.
    WHEN ' '.
      SET PROPERTY OF CELULA  'NumberFormat' = '@'.
  ENDCASE.
  SET PROPERTY OF CELULA  'Value' = VALOR.
ENDFORM.

FORM PREENCHE_DETALHE USING LINHA
                            COLUNA
                            BOLD
                            LARGURA
                            TAM
                            ALINHAMENTO
                            CONTEUDO
                            CAMPO.

  CALL METHOD OF GS_EXCEL 'Cells' = GS_CELL1
  EXPORTING
  #1 = LINHA
  #2 = COLUNA.

  PERFORM PREENCHE_CELULA USING GS_CELL1 BOLD ALINHAMENTO
                                LARGURA TAM CONTEUDO CAMPO.
ENDFORM.
