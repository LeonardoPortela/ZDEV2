************************************************************************
* Programa   : ZMMR047                                                 *
* Descrição  : Requisição de Compras - contas de Celular               *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Antonio Luiz Rodrigues da Silva                         *
* Data       : 30.12.2014                                             *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*


REPORT  ZMMR047.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF TY_ZMMT0024.
        INCLUDE STRUCTURE ZMMT0024.
TYPES:  PERC TYPE ZRC_PERC2,
        QTDE TYPE I,
        END OF TY_ZMMT0024.
************************************************************************
* Tabelas Internas
************************************************************************


DATA:
  IT_ZMMT0024               TYPE TABLE OF TY_ZMMT0024,
  IT_ZMMT0024_TOT           TYPE TABLE OF TY_ZMMT0024,
  IT_ZMMT0024_OBJ           TYPE TABLE OF TY_ZMMT0024,
  TI_ZLEST0100              TYPE TABLE OF ZLEST0100  WITH HEADER LINE,

  WA_ZMMT0024               TYPE TY_ZMMT0024,
  WA_ZMMT0024_TOT           TYPE TY_ZMMT0024,
  WA_ZMMT0024_OBJ           TYPE TY_ZMMT0024,
  WA_LFA1_FORN              TYPE LFA1,
  WA_LFA1                   TYPE LFA1,
  WA_ASMD                   TYPE ASMD,
  WA_ASMDT                  TYPE ASMDT,
  WA_ZLEST0100              TYPE ZLEST0100,

  IT_RETURN_PED             TYPE STANDARD TABLE OF BAPIRET2, "TABLE OF BAPIRET2 WITH HEADER LINE,
  WA_RETURN_PED             TYPE BAPIRET2,
  IT_POITEM_PED             TYPE STANDARD TABLE OF BAPIMEPOITEM, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
  WA_POITEM_PED             TYPE BAPIMEPOITEM,
  IT_POITEMX_PED            TYPE STANDARD TABLE OF BAPIMEPOITEMX,
  WA_POITEMX_PED            TYPE BAPIMEPOITEMX,
  WA_POHEADER_PED           TYPE BAPIMEPOHEADER,
  WA_POHEADERX_PED          TYPE BAPIMEPOHEADERX,
  IT_BAPIMEPOTEXTHEADER_PED TYPE STANDARD TABLE OF BAPIMEPOTEXTHEADER,
  WA_BAPIMEPOTEXTHEADER_PED TYPE BAPIMEPOTEXTHEADER,
  IT_BAPIMEPOACCOUNT        TYPE STANDARD TABLE OF BAPIMEPOACCOUNT,
  WA_BAPIMEPOACCOUNT        TYPE BAPIMEPOACCOUNT,
  IT_POSERVICES	            TYPE STANDARD TABLE OF BAPIESLLC,
  WA_POSERVICES	            TYPE BAPIESLLC,
  IT_POSRVACCESSVALUES      TYPE STANDARD TABLE OF BAPIESKLC,
  WA_POSRVACCESSVALUES      TYPE BAPIESKLC,
  IT_BAPIMEPOACCOUNTX       TYPE STANDARD TABLE OF BAPIMEPOACCOUNTX,
  WA_BAPIMEPOACCOUNTX       TYPE BAPIMEPOACCOUNTX,
  PURCHASEORDER             LIKE BAPIMEPOHEADER-PO_NUMBER,
  V_EKORG                   TYPE T024W-EKORG,
  V_BUKRS                   TYPE J_1BBRANCH-BUKRS,

  IT_OUTRETURN              TYPE TABLE OF ZFIE_RET_DOCUMENT,
  WA_OUTRETURN              TYPE ZFIE_RET_DOCUMENT,

  VG_INDEX                  TYPE SY-TABIX,
  VTYPE                     TYPE BAPIRETURN-TYPE,

  V_LIFNR                   TYPE LFA1-LIFNR,
  V_PREQ_ITEM               TYPE BAPIEBANC-PREQ_ITEM,

  WL_TEXTO                  TYPE LINE OF CATSXT_LONGTEXT_ITAB,

  WA_T001W                  TYPE T001W.


DATA: W_NUMBER    LIKE  BAPIEBANC-PREQ_NO,
      VSERIAL     TYPE  DZEBKN,
      VL_PONTEIRO TYPE ZLEST0100-CONT.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS:
  C_E   TYPE C VALUE 'I',
  C_X   TYPE C VALUE 'X',
  C_MM  TYPE ZFIE_RET_DOCUMENT-ID         VALUE 'MM',
  C_899 TYPE ZFIE_RET_DOCUMENT-NUM        VALUE '899',
  C_29  TYPE ZFIE_RET_DOCUMENT-INTERFACE  VALUE '29',
  C_40  TYPE ZFIE_RET_DOCUMENT-INTERFACE  VALUE '40',
  C_41  TYPE ZFIE_RET_DOCUMENT-INTERFACE  VALUE '41'.

DATA VMSG(50).

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS    : P_OBJKEY TYPE ZMMT0024-OBJKEY .
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  DATA: VG_JOB      TYPE I.

  SELECT SINGLE COUNT( * ) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'PEDIDO_COMEX'
     AND STATUS EQ 'R'.

  IF ( VG_JOB EQ 1 ).
    PERFORM: Z_PROCESSA_PEDIDO,  " Seleção de Dados e criação de pedidos
             Z_PROCESSA_ESTORNO, " Selação de Dados - Estorno
             Z_ENVIA_LOG_LEGADO. " Envia retorno de msg para o legado

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona Dados
*----------------------------------------------------------------------*
FORM Z_PROCESSA_PEDIDO .

  DATA: VMENSAGEM  TYPE STRING,
        LC_PCKG_NO TYPE PACKNO,
        LC_LINE_NO TYPE SRV_LINE_NO,
        WA_EINA    TYPE EINA.

  IF P_OBJKEY IS INITIAL.
    P_OBJKEY = '%'.
  ENDIF.

  SELECT *
    FROM ZMMT0024
    INTO TABLE IT_ZMMT0024
   WHERE EBELN         EQ ''
     AND BSART         IN ('PCE')
     AND OBJKEY        LIKE P_OBJKEY
     AND RG_ATUALIZADO EQ 'N'
     AND ID_INTERFACE  EQ 2.

  CHECK IT_ZMMT0024[] IS NOT INITIAL.


  IT_ZMMT0024_TOT[] = IT_ZMMT0024[].
  SORT: IT_ZMMT0024_TOT BY OBJKEY  BNFPO SAKNR MATNR SRVPOS,
        IT_ZMMT0024     BY OBJKEY  BNFPO SAKNR MATNR SRVPOS.

*  DELETE ADJACENT DUPLICATES FROM IT_ZMMT0024_TOT COMPARING OBJKEY SAKNR MATNR SRVPOS.
*
*  " totalizar quantidade para repetir o lancamento em caso de 1 centro custo por objkey
*  LOOP AT IT_ZMMT0024_TOT INTO WA_ZMMT0024_TOT.
*    VG_INDEX              = SY-TABIX.
*    WA_ZMMT0024_TOT-QTDE = 0.
*    LOOP AT IT_ZMMT0024 INTO WA_ZMMT0024 WHERE OBJKEY       = WA_ZMMT0024_TOT-OBJKEY
*                                         AND   NR_DOCUMENTO = WA_ZMMT0024_TOT-NR_DOCUMENTO.
*      ADD 1 TO WA_ZMMT0024_TOT-QTDE.
*    ENDLOOP.
*    MODIFY IT_ZMMT0024_TOT FROM WA_ZMMT0024_TOT INDEX VG_INDEX TRANSPORTING QTDE.
*  ENDLOOP.
*
*
*  IT_ZMMT0024_TOT[] = IT_ZMMT0024[].
*
*  SORT IT_ZMMT0024_TOT BY OBJKEY SAKNR MATNR SRVPOS.
*  DELETE ADJACENT DUPLICATES FROM IT_ZMMT0024_TOT COMPARING ALL FIELDS.

*  "Totalizar requisição
*  LOOP AT IT_ZMMT0024_TOT INTO WA_ZMMT0024_TOT.
*    WA_ZMMT0024_TOT-BRTWR = 0.
*    WA_ZMMT0024_TOT-MENGE = 0.
*    VG_INDEX              = SY-TABIX.
*    LOOP AT IT_ZMMT0024 INTO WA_ZMMT0024 WHERE OBJKEY       = WA_ZMMT0024_TOT-OBJKEY
*                                         AND   SAKNR        = WA_ZMMT0024_TOT-SAKNR
*                                         AND   MATNR        = WA_ZMMT0024_TOT-MATNR
*                                         AND   SRVPOS       = WA_ZMMT0024_TOT-SRVPOS.
*
**      ADD WA_ZMMT0024-BRTWR TO WA_ZMMT0024_TOT-BRTWR.
*      WA_ZMMT0024_TOT-BRTWR = WA_ZMMT0024_TOT-BRTWR + ( WA_ZMMT0024-BRTWR * WA_ZMMT0024-MENGE ).
*      ADD WA_ZMMT0024-MENGE TO WA_ZMMT0024_TOT-MENGE.
*    ENDLOOP.
*    MODIFY IT_ZMMT0024_TOT FROM WA_ZMMT0024_TOT INDEX VG_INDEX TRANSPORTING BRTWR MENGE.
*  ENDLOOP.
*  " Distribuir percentual
*  LOOP AT IT_ZMMT0024_TOT INTO WA_ZMMT0024_TOT.
*    LOOP AT IT_ZMMT0024 INTO WA_ZMMT0024 WHERE OBJKEY       = WA_ZMMT0024_TOT-OBJKEY
*                                         AND   SAKNR        = WA_ZMMT0024_TOT-SAKNR
*                                         AND   MATNR        = WA_ZMMT0024_TOT-MATNR
*                                         AND   SRVPOS       = WA_ZMMT0024_TOT-SRVPOS.
*      WA_ZMMT0024-PERC =  WA_ZMMT0024-BRTWR / WA_ZMMT0024_TOT-BRTWR.
*      MODIFY IT_ZMMT0024 FROM WA_ZMMT0024 INDEX SY-TABIX TRANSPORTING PERC.
*    ENDLOOP.
*  ENDLOOP.
*  " totalizar percentual
*  LOOP AT IT_ZMMT0024_TOT INTO WA_ZMMT0024_TOT.
*    VG_INDEX              = SY-TABIX.
*    WA_ZMMT0024_TOT-PERC = 0.
*    WA_ZMMT0024_TOT-QTDE = 0.
*    LOOP AT IT_ZMMT0024 INTO WA_ZMMT0024 WHERE OBJKEY       = WA_ZMMT0024_TOT-OBJKEY
*                                         AND   SAKNR        = WA_ZMMT0024_TOT-SAKNR
*                                         AND   MATNR        = WA_ZMMT0024_TOT-MATNR
*                                         AND   SRVPOS       = WA_ZMMT0024_TOT-SRVPOS.
*      ADD WA_ZMMT0024-PERC TO WA_ZMMT0024_TOT-PERC.
*      ADD 1                TO WA_ZMMT0024_TOT-QTDE.
*    ENDLOOP.
*    MODIFY IT_ZMMT0024_TOT FROM WA_ZMMT0024_TOT INDEX VG_INDEX TRANSPORTING PERC QTDE.
*  ENDLOOP.
*
*  "Ajustar
*  LOOP AT IT_ZMMT0024_TOT INTO WA_ZMMT0024_TOT.
*    IF WA_ZMMT0024_TOT-PERC NE 1.
*      LOOP AT IT_ZMMT0024 INTO WA_ZMMT0024 WHERE OBJKEY       = WA_ZMMT0024_TOT-OBJKEY
*                                           AND   SAKNR        = WA_ZMMT0024_TOT-SAKNR
*                                           AND   MATNR        = WA_ZMMT0024_TOT-MATNR
*                                           AND   SRVPOS       = WA_ZMMT0024_TOT-SRVPOS.
*        WA_ZMMT0024-PERC = WA_ZMMT0024-PERC + ( 1 - WA_ZMMT0024_TOT-PERC ) .
*        MODIFY IT_ZMMT0024 FROM WA_ZMMT0024 INDEX SY-TABIX TRANSPORTING PERC.
*        EXIT.
*      ENDLOOP.
*    ENDIF.
*  ENDLOOP.
*  "
  "
*  IT_ZMMT0024_OBJ[] = IT_ZMMT0024_TOT.
*  SORT: IT_ZMMT0024_OBJ BY OBJKEY.
*  DELETE ADJACENT DUPLICATES FROM IT_ZMMT0024_OBJ COMPARING OBJKEY.
*
*  LOOP AT IT_ZMMT0024_OBJ INTO WA_ZMMT0024_OBJ.
*    VG_INDEX = SY-TABIX.
*    WA_ZMMT0024_OBJ-BRTWR = 0.
*    LOOP AT IT_ZMMT0024_TOT INTO WA_ZMMT0024_TOT WHERE OBJKEY       = WA_ZMMT0024_OBJ-OBJKEY.
*      ADD WA_ZMMT0024_TOT-BRTWR TO WA_ZMMT0024_OBJ-BRTWR.
*    ENDLOOP.
*    MODIFY IT_ZMMT0024_OBJ FROM WA_ZMMT0024_OBJ INDEX VG_INDEX TRANSPORTING BRTWR.
*  ENDLOOP.

  IT_ZMMT0024_OBJ[] = IT_ZMMT0024_TOT.
  SORT: IT_ZMMT0024_OBJ BY OBJKEY.
  DELETE ADJACENT DUPLICATES FROM IT_ZMMT0024_OBJ COMPARING OBJKEY.


  LOOP AT IT_ZMMT0024_OBJ INTO WA_ZMMT0024_OBJ.
    CLEAR: WA_LFA1_FORN.
    REFRESH: IT_RETURN_PED, IT_POITEM_PED, IT_POITEMX_PED, IT_BAPIMEPOTEXTHEADER_PED,
             IT_BAPIMEPOACCOUNT,IT_BAPIMEPOACCOUNTX, IT_POSERVICES, IT_POSRVACCESSVALUES.
    CLEAR: WA_RETURN_PED, WA_POITEM_PED, WA_POITEMX_PED,
           WA_POHEADER_PED, WA_POHEADERX_PED, WA_BAPIMEPOTEXTHEADER_PED, WA_POSERVICES, WA_POSRVACCESSVALUES.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZMMT0024_OBJ-FLIEF
      IMPORTING
        OUTPUT = V_LIFNR.

    SELECT SINGLE * INTO WA_LFA1_FORN FROM LFA1 WHERE LIFNR EQ V_LIFNR.

    WA_POHEADER_PED-COMP_CODE = WA_ZMMT0024_OBJ-BUKRS.    "Empresa
    WA_POHEADER_PED-DOC_TYPE = 'PCE'.
    WA_POHEADER_PED-VENDOR   = V_LIFNR.                   "Fornecedor pela ZMM0045

    SELECT SINGLE EKORG
      FROM T024W
      INTO V_EKORG
      WHERE WERKS EQ WA_ZMMT0024_OBJ-WERKS.

    WA_POHEADER_PED-PURCH_ORG = V_EKORG.                  "Organização de Compras
    WA_POHEADER_PED-DOC_DATE  = SY-DATUM.                 "Data do Pedido
    WA_POHEADER_PED-LANGU     = SY-LANGU.                 "Idioma
    WA_POHEADER_PED-PUR_GROUP = 'D14'. " WA_ZMMT0024_OBJ-EKGRP.    "Grupo de Compradores
    WA_POHEADER_PED-CURRENCY  = 'BRL'.          "
    WA_POHEADER_PED-EXCH_RATE = 1.                        "Taxa de Câmbio pela ZMM0045
    WA_POHEADER_PED-OUR_REF   = ''.                       "Safra
    WA_POHEADER_PED-QUOT_DATE = ''.                       "data cotação

    WA_POHEADERX_PED-COMP_CODE = 'X'.                     "Empresa
    WA_POHEADERX_PED-DOC_TYPE  = 'X'.                     "Tipo de Pedido
    WA_POHEADERX_PED-VENDOR    = 'X'.                     "Fornecedor pela ZMM0045
    WA_POHEADERX_PED-PURCH_ORG = 'X'.                     "Organização de Compras
    WA_POHEADERX_PED-DOC_DATE  = 'X'.                     "Data do Pedido
    WA_POHEADERX_PED-LANGU     = 'X'.                     "Idioma
    WA_POHEADERX_PED-PUR_GROUP = 'X'.                     "Grupo de Compradores
    WA_POHEADERX_PED-CURRENCY  = 'X'.                     "Moeda pela ZMM0045
    WA_POHEADERX_PED-EXCH_RATE = 'X'.                     "Taxa pela ZMM0045
    WA_POHEADERX_PED-OUR_REF   = 'X'.                     "Safra
    WA_POHEADERX_PED-QUOT_DATE = 'X'.                     "Data Cotação

    " TEXTOS
    WA_BAPIMEPOTEXTHEADER_PED-TEXT_ID = 'F01'.
    WA_BAPIMEPOTEXTHEADER_PED-TEXT_FORM = '*'.
    WA_BAPIMEPOTEXTHEADER_PED-TEXT_LINE =  'VENCIMENTO|REFERENCIA   |NOTA'.
    APPEND WA_BAPIMEPOTEXTHEADER_PED TO IT_BAPIMEPOTEXTHEADER_PED.
    "
    WA_BAPIMEPOTEXTHEADER_PED-TEXT_LINE =  WA_ZMMT0024_TOT-TXZ01.
    APPEND WA_BAPIMEPOTEXTHEADER_PED TO IT_BAPIMEPOTEXTHEADER_PED.

    CONCATENATE 'CONTA:' WA_ZMMT0024_TOT-BEDNR  INTO WL_TEXTO SEPARATED BY SPACE.
    WA_BAPIMEPOTEXTHEADER_PED-TEXT_LINE =  WL_TEXTO.
    APPEND WA_BAPIMEPOTEXTHEADER_PED TO IT_BAPIMEPOTEXTHEADER_PED.
    "
    V_PREQ_ITEM = 0.
    LC_PCKG_NO = 1.
    LC_LINE_NO = 1.
    LOOP AT IT_ZMMT0024_TOT INTO WA_ZMMT0024_TOT WHERE OBJKEY = WA_ZMMT0024_OBJ-OBJKEY.
      CLEAR: WA_POITEM_PED, WA_POITEMX_PED, WA_POSERVICES,WA_POSRVACCESSVALUES.
      "Serviço
      ADD 10 TO V_PREQ_ITEM.
      IF WA_ZMMT0024_TOT-SRVPOS IS NOT INITIAL.
        WA_ZMMT0024_TOT-PSTYP = '9'.
      ENDIF.
      IF WA_ZMMT0024_TOT-PSTYP = '9'.
        LC_LINE_NO = 1.
        SELECT SINGLE * INTO WA_ASMD FROM ASMD WHERE ASNUM EQ WA_ZMMT0024_TOT-SRVPOS.
        SELECT SINGLE * INTO WA_ASMDT FROM ASMDT WHERE ASNUM EQ WA_ASMD-ASNUM AND SPRAS EQ SY-LANGU.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'SERVICE'
          IMPORTING
            NUMBER                  = LC_PCKG_NO
          EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        "Item do pedido
        WA_POITEM_PED-PCKG_NO        = LC_PCKG_NO.
        WA_POITEMX_PED-PCKG_NO       = 'X'.

        WA_POSERVICES-PCKG_NO    = WA_POITEM_PED-PCKG_NO.

        "incrementa novamente
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'SERVICE'
          IMPORTING
            NUMBER                  = LC_PCKG_NO
          EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        WA_POSERVICES-SUBPCKG_NO = LC_PCKG_NO.
        WA_POSERVICES-LINE_NO    = LC_LINE_NO.
        WA_POSERVICES-PLN_LINE   = LC_LINE_NO.
        APPEND WA_POSERVICES TO IT_POSERVICES.

        CLEAR: WA_POSERVICES.
        ADD 1 TO LC_LINE_NO.
        WA_POSERVICES-PCKG_NO    = LC_PCKG_NO.
        WA_POSERVICES-LINE_NO    = LC_LINE_NO.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = V_PREQ_ITEM
          IMPORTING
            OUTPUT = WA_POSERVICES-EXT_LINE.

        WA_POSERVICES-SERVICE    = WA_ASMD-ASNUM.
        WA_POSERVICES-SHORT_TEXT = WA_ASMDT-ASKTX.
*        WA_POSERVICES-QUANTITY   = 1.
        WA_POSERVICES-QUANTITY   = WA_ZMMT0024_TOT-MENGE.
        WA_POSERVICES-PRICE_UNIT = 1.
        WA_POSERVICES-GR_PRICE   = ( WA_ZMMT0024_TOT-BRTWR / WA_ZMMT0024_TOT-MENGE ).
        APPEND WA_POSERVICES TO IT_POSERVICES.

      ENDIF.
      "_ITEMS
"*---> 05/07/2023 - Migração S4 - LO
*      WA_POITEM_PED-PO_ITEM    = V_PREQ_ITEM.                               "Item
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_ZMMT0024_TOT-MATNR                                    "Material
*        IMPORTING
*          OUTPUT = WA_POITEM_PED-MATERIAL.

   DATA(v_mat) = |{ WA_ZMMT0024_TOT-MATNR ALPHA = IN  }|.
   DATA(v_len) = strlen( v_mat ).

   IF v_len > 18.
     WA_POITEM_PED-material_long = v_mat.
   ELSE.
     WA_POITEM_PED-MATERIAL      = v_mat.
   ENDIF.
   CLEAR: v_mat, v_len.
"*<--- 05/07/2023 - Migração S4 - LO
      WA_POITEM_PED-QUANTITY   = WA_ZMMT0024_TOT-MENGE.                      "Quantidade
      WA_POITEM_PED-PO_PRICE   = 1.                                         "Transferência do preço: 1 = bruto, 2 = líquido
      WA_POITEM_PED-NET_PRICE  = WA_ZMMT0024_TOT-BRTWR.                     "Preço
      IF WA_ZMMT0024_TOT-PSTYP = '9'.
        WA_POITEM_PED-TAX_CODE   = 'S0'.
        WA_POITEM_PED-QUANTITY   = 1.
      ELSE.
        WA_POITEM_PED-TAX_CODE   = 'C0'.                                    "Código do Imposto
        WA_POITEM_PED-NET_PRICE  = WA_ZMMT0024_TOT-BRTWR / WA_ZMMT0024_TOT-MENGE.
      ENDIF.
      WA_POITEM_PED-PLANT      = WA_ZMMT0024_TOT-WERKS.                     "Centro
      WA_POITEM_PED-STGE_LOC   = ''.                                        "Depósito
      WA_POITEM_PED-ACCTASSCAT = WA_ZMMT0024_TOT-KNTTP.                     "
      WA_POITEM_PED-ITEM_CAT   = WA_ZMMT0024_TOT-PSTYP.                     "
      IF WA_ZMMT0024_TOT-QTDE GT 1.
        WA_POITEM_PED-DISTRIB   = 2.
        WA_POITEM_PED-PART_INV  = 2.
      ENDIF.
      APPEND WA_POITEM_PED TO IT_POITEM_PED.
      WA_POITEMX_PED-PO_ITEM    = V_PREQ_ITEM.                               "Item
      WA_POITEMX_PED-PO_ITEMX   = 'X'.                                       "Item
      WA_POITEMX_PED-MATERIAL   = 'X'.                                       "Material
      WA_POITEMX_PED-QUANTITY   = 'X'.                                       "Quantidade
      WA_POITEMX_PED-PO_PRICE   = 'X'.                                       "Transferência do preço: 1 = bruto, 2 = líquido
      WA_POITEMX_PED-NET_PRICE  = 'X'.                                       "Preço
      WA_POITEMX_PED-TAX_CODE   = 'X'.                                       "Código do Imposto
      WA_POITEMX_PED-PLANT      = 'X'.                                       "Centro
      WA_POITEMX_PED-STGE_LOC   = 'X'.                                       "Depósito
      WA_POITEMX_PED-ACCTASSCAT = 'X'.
      WA_POITEMX_PED-ITEM_CAT   = 'X'.
      IF WA_ZMMT0024_TOT-QTDE GT 1.
        WA_POITEMX_PED-DISTRIB   = 'X'.
        WA_POITEMX_PED-PART_INV  = 'X'.
      ENDIF.
      APPEND WA_POITEMX_PED TO IT_POITEMX_PED.

      VSERIAL = 0.
      LOOP AT IT_ZMMT0024 INTO WA_ZMMT0024 WHERE OBJKEY       = WA_ZMMT0024_TOT-OBJKEY
                                           AND   BNFPO        = WA_ZMMT0024_TOT-BNFPO
                                           AND   SAKNR        = WA_ZMMT0024_TOT-SAKNR
                                           AND   MATNR        = WA_ZMMT0024_TOT-MATNR
                                           AND   SRVPOS       = WA_ZMMT0024_TOT-SRVPOS.
        WA_BAPIMEPOACCOUNT-PO_ITEM  = V_PREQ_ITEM.

        ADD 1 TO VSERIAL.
        WA_BAPIMEPOACCOUNT-SERIAL_NO  = VSERIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_ZMMT0024-SAKNR
          IMPORTING
            OUTPUT = WA_BAPIMEPOACCOUNT-GL_ACCOUNT.

        WA_BAPIMEPOACCOUNT-QUANTITY   = WA_ZMMT0024-PERC.
*        IF WA_ZMMT0024-PERC LT 1.
*          WA_BAPIMEPOACCOUNT-DISTR_PERC = WA_ZMMT0024-PERC * 100.
*        ELSE.
        CLEAR WA_BAPIMEPOACCOUNT-DISTR_PERC .
*        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_ZMMT0024-KOSTL
          IMPORTING
            OUTPUT = WA_BAPIMEPOACCOUNT-COSTCENTER.

        APPEND WA_BAPIMEPOACCOUNT TO IT_BAPIMEPOACCOUNT.
        CLEAR: WA_BAPIMEPOACCOUNT.
        "
        WA_BAPIMEPOACCOUNTX-PO_ITEM    = V_PREQ_ITEM.
        WA_BAPIMEPOACCOUNTX-SERIAL_NO  = VSERIAL.
*        WA_BAPIMEPOACCOUNTX-PO_ITEMX   = 'X'.
        WA_BAPIMEPOACCOUNTX-GL_ACCOUNT = 'X'.
        WA_BAPIMEPOACCOUNTX-QUANTITY   = 'X'.
*        IF WA_ZMMT0024-PERC LT 1.
*          WA_BAPIMEPOACCOUNTX-DISTR_PERC = 'X'.
*        ELSE.
        CLEAR WA_BAPIMEPOACCOUNTX-DISTR_PERC .
*        ENDIF.
        WA_BAPIMEPOACCOUNTX-COSTCENTER   = 'X'.

        APPEND WA_BAPIMEPOACCOUNTX TO IT_BAPIMEPOACCOUNTX.
        CLEAR: WA_BAPIMEPOACCOUNTX.
        "
        IF WA_ZMMT0024_TOT-PSTYP = '9'.
          WA_POSRVACCESSVALUES-PCKG_NO    = WA_POSERVICES-PCKG_NO.
          WA_POSRVACCESSVALUES-LINE_NO    = WA_POSERVICES-LINE_NO.
          WA_POSRVACCESSVALUES-SERNO_LINE = '01'.
          WA_POSRVACCESSVALUES-SERIAL_NO  = VSERIAL.
*          IF WA_ZMMT0024_TOT-QTDE EQ 1.
          WA_POSRVACCESSVALUES-PERCENTAGE = 100.
*          ELSE.
*            WA_POSRVACCESSVALUES-PERCENTAGE = WA_ZMMT0024-PERC * 100.
*          ENDIF.
*          WA_POSRVACCESSVALUES-QUANTITY  = 1.
          WA_POSRVACCESSVALUES-QUANTITY  = WA_ZMMT0024-MENGE.
          WA_POSRVACCESSVALUES-NET_VALUE = WA_ZMMT0024-BRTWR.
          APPEND WA_POSRVACCESSVALUES TO IT_POSRVACCESSVALUES.
          CLEAR WA_POSRVACCESSVALUES.
        ENDIF.
      ENDLOOP.
*      "
    ENDLOOP.

    CLEAR W_NUMBER.
    IF IT_POSERVICES[] IS INITIAL.
"*---> 05/07/2023 - Migração S4 - LO
      CALL FUNCTION 'BAPI_PO_CREATE1'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          POHEADER         = WA_POHEADER_PED
          POHEADERX        = WA_POHEADERX_PED
        IMPORTING
          EXPPURCHASEORDER = PURCHASEORDER
        TABLES
          RETURN           = IT_RETURN_PED
          POITEM           = IT_POITEM_PED
          POITEMX          = IT_POITEMX_PED
          POTEXTHEADER     = IT_BAPIMEPOTEXTHEADER_PED
          POACCOUNT        = IT_BAPIMEPOACCOUNT
          POACCOUNTX       = IT_BAPIMEPOACCOUNTX.
    ELSE.
"*---> 05/07/2023 - Migração S4 - LO
      CALL FUNCTION 'BAPI_PO_CREATE1'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          POHEADER          = WA_POHEADER_PED
          POHEADERX         = WA_POHEADERX_PED
        IMPORTING
          EXPPURCHASEORDER  = PURCHASEORDER
        TABLES
          RETURN            = IT_RETURN_PED
          POITEM            = IT_POITEM_PED
          POITEMX           = IT_POITEMX_PED
          POTEXTHEADER      = IT_BAPIMEPOTEXTHEADER_PED
          POACCOUNT         = IT_BAPIMEPOACCOUNT
          POACCOUNTX        = IT_BAPIMEPOACCOUNTX
          POSERVICES        = IT_POSERVICES
          POSRVACCESSVALUES = IT_POSRVACCESSVALUES.
    ENDIF.
    CLEAR VTYPE.
    "Verifica se a BAPI foi executada sem erros e commita
    READ TABLE IT_RETURN_PED INTO WA_RETURN_PED WITH KEY TYPE = 'S' ID = '06' NUMBER = '017'.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = C_X.

      VTYPE = 'S'.
      W_NUMBER = PURCHASEORDER.
      UPDATE  ZMMT0024 SET EBELN = W_NUMBER
                           RG_ATUALIZADO = 'S'
      WHERE OBJKEY        = WA_ZMMT0024_OBJ-OBJKEY.
      COMMIT WORK.

      "
      CONCATENATE 'Contrato ' WA_ZMMT0024_OBJ-OBJKEY 'Pedido.' W_NUMBER INTO VMSG SEPARATED BY SPACE.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          TEXT = VMSG.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CONCATENATE 'Contrato ' WA_ZMMT0024_OBJ-OBJKEY 'não gerado pedido.'  INTO VMSG SEPARATED BY SPACE.
      UPDATE  ZMMT0024 SET RG_ATUALIZADO = 'E'
          WHERE OBJKEY        = WA_ZMMT0024_OBJ-OBJKEY.
      COMMIT WORK.
    ENDIF.

    REFRESH TI_ZLEST0100.
    CLEAR VL_PONTEIRO.
    SELECT  MAX( CONT )
       FROM ZLEST0100
       INTO VL_PONTEIRO
       WHERE CH_REFERENCIA = WA_ZMMT0024_OBJ-OBJKEY.
    IF SY-SUBRC = 0.
      ADD 1 TO VL_PONTEIRO.
    ELSE.
      VL_PONTEIRO = 1.
    ENDIF.
    LOOP AT IT_RETURN_PED INTO WA_RETURN_PED.
      WA_ZLEST0100-MANDT      = SY-MANDT.
      WA_ZLEST0100-CH_REFERENCIA   = WA_ZMMT0024_OBJ-OBJKEY.
      WA_ZLEST0100-MSGTYP     = WA_RETURN_PED-TYPE.
      WA_ZLEST0100-MSGSPRA    = SY-LANGU.
      WA_ZLEST0100-MSGID      = WA_RETURN_PED-ID.
      WA_ZLEST0100-MSGNR      = WA_RETURN_PED-NUMBER.
      WA_ZLEST0100-MSGV1      = WA_RETURN_PED-MESSAGE.
      WA_ZLEST0100-DATA       = SY-DATUM.
      WA_ZLEST0100-HORA       = SY-UZEIT.
      WA_ZLEST0100-USUARIO    = SY-UNAME.
      WA_ZLEST0100-CONT       = VL_PONTEIRO.

      APPEND WA_ZLEST0100 TO TI_ZLEST0100.
      ADD 1 TO VL_PONTEIRO.
      IF VTYPE = 'S'.
        WA_RETURN_PED-MESSAGE_V1 = PURCHASEORDER.
      ELSE.
        VTYPE = WA_RETURN_PED-TYPE.
      ENDIF.
      PERFORM Z_PREPARA_MENSAGEM USING WA_ZMMT0024_OBJ-OBJKEY VTYPE C_40 WA_RETURN_PED-MESSAGE WA_RETURN_PED-MESSAGE_V1 .
    ENDLOOP.
    IF TI_ZLEST0100[] IS NOT INITIAL.
      MODIFY ZLEST0100 FROM TABLE TI_ZLEST0100.
      COMMIT WORK.
    ENDIF.

  ENDLOOP.

ENDFORM.             " Z_SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
FORM Z_PREPARA_MENSAGEM USING POBJ_KEY
                              PTYPE
                              INTERFACE
                              PMESSAGE
                              PMESSAGE_V1 .

  CLEAR WA_OUTRETURN.

  WA_OUTRETURN-OBJ_KEY        = POBJ_KEY.
  WA_OUTRETURN-INTERFACE      = INTERFACE.
  WA_OUTRETURN-DT_ATUALIZACAO = SY-DATUM.
  WA_OUTRETURN-HR_ATUALIZACAO = SY-UZEIT.
  WA_OUTRETURN-TYPE           = PTYPE.
  WA_OUTRETURN-ID             = C_MM.
  WA_OUTRETURN-NUM            = C_899.
  WA_OUTRETURN-MESSAGE        = PMESSAGE.
  WA_OUTRETURN-MESSAGE_V1     = PMESSAGE_V1.

  APPEND WA_OUTRETURN TO IT_OUTRETURN.


ENDFORM.                    "z_prepara_mensagem

*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*       Envia log para o legado
*----------------------------------------------------------------------*
FORM Z_ENVIA_LOG_LEGADO .

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  IF NOT IT_OUTRETURN[] IS INITIAL.
    SORT IT_OUTRETURN BY OBJ_KEY INTERFACE.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        OUTRETURN = IT_OUTRETURN.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = IT_OUTRETURN.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = IT_OUTRETURN.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    COMMIT WORK.

  ENDIF.

ENDFORM.                    " Z_ENVIA_LOG_LEGADO

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_COMEX_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM Z_PROCESSA_ESTORNO .

  DATA: IT_ZMMT0024_ORG    TYPE TABLE OF ZMMT0024 WITH HEADER LINE.


  DATA: WA_PURCHASEORDER LIKE BAPIMEPOHEADER-PO_NUMBER,
        WA_POHEADER      LIKE BAPIMEPOHEADER,
        WA_POHEADERX     LIKE BAPIMEPOHEADERX.

  FIELD-SYMBOLS: <FS0024> TYPE TY_ZMMT0024.

  CLEAR: IT_ZMMT0024.

  IF P_OBJKEY IS INITIAL.
    P_OBJKEY = '%'.
  ENDIF.

  "Requisições de Compra COMEX
  SELECT *
    FROM ZMMT0024
    INTO TABLE IT_ZMMT0024
   WHERE EBELN         EQ ''
     AND OBJKEY        LIKE P_OBJKEY
     AND RG_ATUALIZADO EQ 'N'
     AND ID_INTERFACE  EQ 3.

  CHECK SY-SUBRC IS INITIAL.

  " Já marca como lido (se houver erro a interface enviara as informações novamente)
  UPDATE ZMMT0024
     SET RG_ATUALIZADO = 'S'
   WHERE EBELN         EQ ''
     AND OBJKEY        LIKE P_OBJKEY
     AND RG_ATUALIZADO EQ 'N'
     AND ID_INTERFACE  EQ 3.

  COMMIT WORK.

  SELECT * INTO TABLE IT_ZMMT0024_ORG
    FROM ZMMT0024
     FOR ALL ENTRIES IN IT_ZMMT0024
   WHERE OBJKEY EQ IT_ZMMT0024-OBJKEY
     AND EBELN  NE SPACE.

  SORT IT_ZMMT0024_ORG BY OBJKEY NR_DOCUMENTO.
  DELETE ADJACENT DUPLICATES FROM IT_ZMMT0024_ORG COMPARING OBJKEY NR_DOCUMENTO.

  LOOP AT IT_ZMMT0024 INTO WA_ZMMT0024.
    "
    READ TABLE IT_ZMMT0024_ORG WITH KEY OBJKEY       = WA_ZMMT0024-OBJKEY
                                        NR_DOCUMENTO = WA_ZMMT0024-NR_DOCUMENTO BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      " Verificar Pedido de Compra """"""""""""""""""""""""""""""""""""""""""""""""""""
      WA_PURCHASEORDER       = IT_ZMMT0024_ORG-EBELN.
      WA_POHEADER-PO_NUMBER  = IT_ZMMT0024_ORG-EBELN.
      WA_POHEADER-DELETE_IND = 'X'.

      WA_POHEADERX-PO_NUMBER  = 'X'.
      WA_POHEADERX-DELETE_IND = 'X'.

"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_PO_CHANGE'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          PURCHASEORDER = WA_PURCHASEORDER
          POHEADER      = WA_POHEADER
          POHEADERX     = WA_POHEADERX
        TABLES
          RETURN        = IT_RETURN_PED.

      LOOP AT IT_RETURN_PED INTO WA_RETURN_PED.
        IF WA_RETURN_PED-TYPE = 'I' .
          VTYPE = 'S'.
        ELSE.
          VTYPE = WA_RETURN_PED-TYPE.
        ENDIF.
        PERFORM Z_PREPARA_MENSAGEM USING WA_ZMMT0024-OBJKEY VTYPE C_41 WA_RETURN_PED-MESSAGE WA_RETURN_PED-MESSAGE_V1 .
      ENDLOOP.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    ELSE.
      VTYPE             = 'E'.
      WA_RETURN_PED-MESSAGE_V1 = WA_ZMMT0024-OBJKEY.
      CONCATENATE 'Não encontrado pedido para chave ' WA_ZMMT0024-OBJKEY WA_ZMMT0024-NR_DOCUMENTO INTO WA_RETURN_PED-MESSAGE.
      PERFORM Z_PREPARA_MENSAGEM USING WA_ZMMT0024-OBJKEY VTYPE C_41 WA_RETURN_PED-MESSAGE WA_RETURN_PED-MESSAGE_V1.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " Z_PROCESSA_COMEX_ESTORNO
