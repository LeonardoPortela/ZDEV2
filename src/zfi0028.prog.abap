*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 26/08/2013                                              &*
*& Descrição:                                                         &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZFI0028.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

TABLES: EKKO,
        EKPO.

TYPES: BEGIN OF TY_EKKO,
        BUKRS TYPE EKKO-BUKRS,
        EKGRP TYPE EKKO-EKGRP,
        AEDAT TYPE EKKO-AEDAT,
        EBELN TYPE EKKO-EBELN,
        LIFNR TYPE EKKO-LIFNR,
      END OF TY_EKKO,

      BEGIN OF TY_EKPO,
        EBELN TYPE EKPO-EBELN,
        WERKS TYPE EKPO-WERKS,
        ELIKZ TYPE EKPO-ELIKZ,
        TXZ01 TYPE EKPO-TXZ01,
        EBELP TYPE EKPO-EBELP,
        BUKRS TYPE EKPO-BUKRS,
      END OF TY_EKPO,

      BEGIN OF TY_EKBE,
        EBELN TYPE EKBE-EBELN,
        EBELP TYPE EKBE-EBELP,
        VGABE TYPE EKBE-VGABE,
        WRBTR TYPE EKBE-WRBTR,
        SHKZG TYPE EKBE-SHKZG,
        GJAHR TYPE EKBE-GJAHR,
        BELNR TYPE EKBE-BELNR,
      END OF TY_EKBE,

      BEGIN OF TY_BKPF,
        BUKRS TYPE BKPF-BUKRS,
        AWKEY TYPE BKPF-AWKEY,
        GJAHR TYPE BKPF-GJAHR,
        BELNR TYPE BKPF-BELNR,
        XBLNR TYPE BKPF-XBLNR,
     END OF TY_BKPF,

     BEGIN OF TY_EKBE_EKKO,
       BUKRS TYPE EKKO-BUKRS,
       AWKEY TYPE BKPF-AWKEY,
       GJAHR TYPE EKBE-GJAHR,
     END OF TY_EKBE_EKKO,

     BEGIN OF TY_EKBE_EKKO_BKPF,
       BUKRS TYPE EKKO-BUKRS,
       BELNR TYPE BKPF-BELNR,
       GJAHR TYPE EKBE-GJAHR,
     END OF TY_EKBE_EKKO_BKPF,

     BEGIN OF TY_RBKP,
       BELNR TYPE RBKP-BELNR,
       GJAHR TYPE RBKP-GJAHR,
     END OF  TY_RBKP,

     BEGIN OF TY_WITH_ITEM,
       BUKRS    TYPE WITH_ITEM-BUKRS,
       BELNR    TYPE WITH_ITEM-BELNR,
       GJAHR    TYPE WITH_ITEM-GJAHR,
       WITHT    TYPE WITH_ITEM-WITHT,
       WT_QBSHH TYPE WITH_ITEM-WT_QBSHH,
     END OF TY_WITH_ITEM,

     BEGIN OF TY_LFA1,
       NAME1 TYPE LFA1-NAME1,
       LIFNR TYPE LFA1-LIFNR,
     END OF TY_LFA1,

      BEGIN OF TY_SAIDA,
        EBELN     TYPE EKPO-EBELN,
        EBELP     TYPE EKPO-EBELP,
        LIFNR     TYPE EKKO-LIFNR,
        NAME1     TYPE LFA1-NAME1,
        TXZ01     TYPE EKPO-TXZ01,
        WERKS     TYPE EKPO-WERKS,
        WRBTR     TYPE EKBE-WRBTR,
        TOT_ADTO  TYPE EKBE-WRBTR,
        SDO_PREST TYPE RBKP-RMWWR,
        MARK,
      END OF TY_SAIDA,

      BEGIN OF TY_EKBE_AUX2,
        BUKRS TYPE BKPF-BUKRS,
        AWKEY TYPE BKPF-AWKEY,
        GJAHR TYPE BKPF-GJAHR,
      END OF TY_EKBE_AUX2.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_EKKO           TYPE TABLE OF TY_EKKO,
      T_EKPO           TYPE TABLE OF TY_EKPO,
      T_EKBE           TYPE TABLE OF TY_EKBE,
      T_EKBE_AUX2      TYPE TABLE OF TY_EKBE_AUX2,
      T_EKBE_EKKO_BKPF TYPE TABLE OF TY_EKBE_EKKO_BKPF,
      T_EKBE_AUX       TYPE TABLE OF TY_EKBE,
      T_BKPF           TYPE TABLE OF BKPF,
      T_RBKP           TYPE TABLE OF RBKP,
      T_WITH_ITEM      TYPE TABLE OF WITH_ITEM,
      T_LFA1           TYPE TABLE OF LFA1,
      T_SAIDA          TYPE TABLE OF TY_SAIDA.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_EKKO           TYPE TY_EKKO,
      WA_EKPO           TYPE TY_EKPO,
      WA_EKBE           TYPE TY_EKBE,
      WA_EKBE_AUX       TYPE TY_EKBE,
      WA_EKBE_AUX2      TYPE TY_EKBE_AUX2,
      WA_EKBE_EKKO_BKPF TYPE TY_EKBE_EKKO_BKPF,
      WA_BKPF           TYPE BKPF,
      WA_RBKP           TYPE RBKP,
      WA_WITH_ITEM      TYPE WITH_ITEM,
      WA_LFA1           TYPE LFA1,
      WA_SAIDA          TYPE TY_SAIDA.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: C_X               TYPE C VALUE 'X'.
*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: WG_TOTAL      TYPE WITH_ITEM-WT_QBSHH.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*
" Seleção de Campos (TextField)
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR EKKO-BUKRS," OBLIGATORY,
                S_EKGRP FOR EKKO-EKGRP," OBLIGATORY DEFAULT 'C40',
                S_WERKS FOR EKPO-WERKS," OBLIGATORY,
                S_LIFNR FOR EKKO-LIFNR,
                S_AEDAR FOR EKKO-AEDAT ,"OBLIGATORY,
                S_EBELN FOR EKKO-EBELN.

SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZAR_DADOS.
  PERFORM INICIAR_VARIAVEIS.
  PERFORM IMPRIMIR_DADOS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  SELECT BUKRS EKGRP AEDAT EBELN LIFNR
    FROM EKKO
      INTO TABLE T_EKKO
        WHERE BUKRS IN S_BUKRS
          AND EKGRP IN S_EKGRP
          AND AEDAT IN S_AEDAR
          AND LIFNR IN S_LIFNR
          AND EBELN IN S_EBELN.

  IF SY-SUBRC IS INITIAL.
    SELECT EBELN WERKS ELIKZ TXZ01 EBELP BUKRS
      FROM EKPO
       INTO TABLE T_EKPO
       FOR ALL ENTRIES IN T_EKKO
         WHERE EBELN EQ T_EKKO-EBELN
           AND WERKS IN S_WERKS
           AND ELIKZ EQ SPACE.

    IF SY-SUBRC IS INITIAL.
      SELECT EBELN EBELP VGABE WRBTR SHKZG GJAHR BELNR
        FROM EKBE
          INTO TABLE T_EKBE
          FOR ALL ENTRIES IN T_EKPO
            WHERE EBELN EQ T_EKPO-EBELN
              AND EBELP EQ T_EKPO-EBELP
              AND ( VGABE EQ '4'
               OR   VGABE EQ '2' ). """""""""""""""""""""""""

      """""""""""""""""""""""""""""""""""""""""""""""""""
      T_EKBE_AUX[] = T_EKBE[].
      DELETE T_EKBE_AUX WHERE VGABE NE '2'.

      IF T_EKBE_AUX[] IS NOT INITIAL.
        SELECT *
          FROM RBKP
          INTO TABLE T_RBKP
            FOR ALL ENTRIES IN T_EKBE_AUX
             WHERE BELNR EQ T_EKBE_AUX-BELNR
               AND GJAHR EQ T_EKBE_AUX-GJAHR.

        LOOP AT T_EKBE_AUX INTO WA_EKBE_AUX.
          READ TABLE T_EKPO INTO WA_EKPO
            WITH KEY EBELN = WA_EKBE_AUX-EBELN
                     EBELP = WA_EKBE_AUX-EBELP.

          WA_EKBE_AUX2-BUKRS = WA_EKPO-BUKRS.
          CONCATENATE WA_EKBE_AUX-BELNR WA_EKBE_AUX-GJAHR INTO WA_EKBE_AUX2-AWKEY.
          WA_EKBE_AUX2-GJAHR = WA_EKBE_AUX-GJAHR.

          APPEND WA_EKBE_AUX2 TO T_EKBE_AUX2.
          CLEAR WA_EKBE_AUX2.
        ENDLOOP.

        IF T_EKBE_AUX2[] IS NOT INITIAL.
          SELECT *
            FROM BKPF
            INTO TABLE T_BKPF
             FOR ALL ENTRIES IN T_EKBE_AUX2
             WHERE AWKEY EQ T_EKBE_AUX2-AWKEY
               AND BUKRS EQ T_EKBE_AUX2-BUKRS
               AND GJAHR EQ T_EKBE_AUX2-GJAHR.

          IF SY-SUBRC IS INITIAL.
            SELECT *
              FROM WITH_ITEM
               INTO TABLE T_WITH_ITEM
               FOR ALL ENTRIES IN T_BKPF
               WHERE BELNR EQ T_BKPF-BELNR
                 AND GJAHR EQ T_BKPF-GJAHR
                 AND BUKRS EQ T_BKPF-BUKRS.
          ENDIF.
        ENDIF.
      ENDIF.
      """""""""""""""""""""""""""""""""""""""""""""""""""
    ENDIF.

  ENDIF.

  SELECT NAME1 LIFNR
    FROM LFA1
      INTO TABLE T_LFA1
      FOR ALL ENTRIES IN T_EKKO
        WHERE LIFNR EQ T_EKKO-LIFNR.

*      LOOP AT T_EKBE_AUX INTO WA_EKBE_AUX.
*        READ TABLE T_EKKO INTO WA_EKKO
*          WITH KEY EBELN = WA_EKBE_AUX-EBELN.
*
*        WA_EKBE_EKKO-BUKRS = WA_EKKO-BUKRS.
*        CONCATENATE WA_EKBE_AUX-BELNR WA_EKBE_AUX-GJAHR INTO WA_EKBE_EKKO-AWKEY.
*        WA_EKBE_EKKO-GJAHR = WA_EKBE_AUX-GJAHR.
*
*        APPEND WA_EKBE_EKKO TO T_EKBE_EKKO.
*        CLEAR: WA_EKBE_EKKO, WA_EKBE, WA_EKKO.
*      ENDLOOP.

*
*      SELECT BUKRS AWKEY GJAHR BELNR XBLNR
*        FROM BKPF
*          INTO TABLE T_BKPF
*          FOR ALL ENTRIES IN T_EKBE_EKKO
*            WHERE BUKRS EQ T_EKBE_EKKO-BUKRS
*              AND AWKEY EQ T_EKBE_EKKO-AWKEY
*              AND GJAHR EQ T_EKBE_EKKO-GJAHR.

*      LOOP AT T_EKBE_EKKO INTO WA_EKBE_EKKO.
*        READ TABLE T_BKPF INTO WA_BKPF
*          WITH KEY BUKRS = WA_BKPF-BUKRS
*                   GJAHR = WA_BKPF-GJAHR.
*        WA_EKBE_EKKO_BKPF-BUKRS = WA_EKBE_EKKO-BUKRS.
*        WA_EKBE_EKKO_BKPF-BELNR = WA_BKPF-BELNR.
*        WA_EKBE_EKKO_BKPF-GJAHR = WA_EKBE_EKKO-GJAHR.
*
*        APPEND WA_EKBE_EKKO_BKPF TO T_EKBE_EKKO_BKPF.
*        CLEAR: WA_EKBE_EKKO_BKPF, WA_BKPF, WA_EKBE_EKKO.
*      ENDLOOP.
*
*      SELECT BUKRS BELNR GJAHR WITHT WT_QBSHH
*        FROM WITH_ITEM
*          INTO TABLE T_WITH_ITEM
*            FOR ALL ENTRIES IN T_EKBE_EKKO_BKPF
*              WHERE BUKRS EQ T_EKBE_EKKO_BKPF-BUKRS
*                AND BELNR EQ T_EKBE_EKKO_BKPF-BELNR
*                AND GJAHR EQ T_EKBE_EKKO_BKPF-GJAHR.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .
  DATA: WL_TOT_ADTO   TYPE EKBE-WRBTR.
  DATA: WL_TOTAL_NF   TYPE WITH_ITEM-WT_QBSHH,
        WL_AWKEY      TYPE BKPF-AWKEY,
        WL_TOTAL_PIS  TYPE WITH_ITEM-WT_QBSHH,
        WL_TOTAL_INSS TYPE WITH_ITEM-WT_QBSHH,
        WL_TOTAL_IR   TYPE WITH_ITEM-WT_QBSHH.

  CLEAR: WL_TOT_ADTO, WA_EKPO, WA_EKKO, WA_EKBE, WA_LFA1,
         WA_WITH_ITEM, WA_EKBE_AUX, WA_EKBE_AUX2, WA_RBKP.

  CLEAR: WL_TOTAL_NF, WL_AWKEY, WL_TOTAL_PIS, WL_TOTAL_INSS, WL_TOTAL_IR, WG_TOTAL.

  LOOP AT T_EKPO INTO WA_EKPO.
    CLEAR: WL_TOT_ADTO.

    LOOP AT T_EKBE INTO WA_EKBE
      WHERE EBELN EQ WA_EKPO-EBELN
        AND EBELP EQ WA_EKPO-EBELP
        AND VGABE EQ '4'.

      IF WA_EKBE-SHKZG NE 'S'.
        WL_TOT_ADTO = (  WA_EKBE-WRBTR  *  -1  ).
      ENDIF.

      WL_TOT_ADTO = ( WL_TOT_ADTO + WA_EKBE-WRBTR ).

      CLEAR: WA_EKBE-WRBTR.
    ENDLOOP.

    READ TABLE T_EKKO INTO WA_EKKO
      WITH KEY EBELN = WA_EKPO-EBELN.

    IF SY-SUBRC IS INITIAL.
      READ TABLE T_LFA1 INTO WA_LFA1
        WITH KEY LIFNR = WA_EKKO-LIFNR.

      WA_SAIDA-NAME1 = WA_LFA1-NAME1.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT T_EKBE_AUX INTO WA_EKBE_AUX
      WHERE EBELN EQ WA_EKPO-EBELN
        AND EBELP EQ WA_EKPO-EBELP
        AND VGABE EQ '2'.

      LOOP AT  T_RBKP INTO WA_RBKP
        WHERE BELNR EQ WA_EKBE_AUX-BELNR
          AND GJAHR EQ WA_EKBE_AUX-GJAHR.

        WL_TOTAL_NF = ( WL_TOTAL_NF + WA_RBKP-RMWWR ).
        CLEAR: WA_RBKP-RMWWR.

      ENDLOOP.
      CONCATENATE WA_EKBE_AUX-BELNR WA_EKBE_AUX-GJAHR INTO WL_AWKEY.

      READ TABLE T_BKPF INTO WA_BKPF
        WITH KEY AWKEY = WL_AWKEY
                 BUKRS = WA_EKKO-BUKRS
                 GJAHR = WA_EKBE_AUX-GJAHR.

      IF SY-SUBRC IS INITIAL.
        LOOP AT T_WITH_ITEM INTO WA_WITH_ITEM
                  WHERE BELNR EQ WA_BKPF-BELNR
                    AND GJAHR EQ WA_BKPF-GJAHR.

          IF WA_WITH_ITEM-WITHT EQ 'CT'.
            WL_TOTAL_PIS = ( WL_TOTAL_PIS + WA_WITH_ITEM-WT_QBSHH ).

          ELSEIF WA_WITH_ITEM-WITHT EQ 'IR'.
            WL_TOTAL_IR = ( WL_TOTAL_IR + WA_WITH_ITEM-WT_QBSHH ).

          ELSEIF WA_WITH_ITEM-WITHT NE 'IR'
             AND WA_WITH_ITEM-WITHT NE 'CT'.
            WL_TOTAL_INSS = ( WL_TOTAL_INSS + WA_WITH_ITEM-WT_QBSHH ).
          ENDIF.
          CLEAR: WA_WITH_ITEM.
        ENDLOOP.


      ENDIF.
    ENDLOOP.
    if wl_total_pis lt '0'.
      wl_total_pis = ( wl_total_pis * ( -1 ) ).
    ENDIF.

    if wl_total_ir lt '0'.
      wl_total_ir = ( wl_total_ir * ( -1 ) ).
    ENDIF.
    if wl_total_inss lt '0'.
      wl_total_inss = ( wl_total_inss * ( -1 ) ).
    ENDIF.

    WL_TOTAL_NF = ( WL_TOTAL_NF - (  ( WL_TOTAL_PIS + WL_TOTAL_IR +
                    WL_TOTAL_INSS ) + WL_TOT_ADTO )                 ).
    """"""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""
    WA_SAIDA-SDO_PREST = WL_TOTAL_NF.
    WA_SAIDA-EBELN    = WA_EKPO-EBELN.
    WA_SAIDA-EBELP    = WA_EKPO-EBELP.
    WA_SAIDA-LIFNR    = WA_EKKO-LIFNR.
    WA_SAIDA-TXZ01    = WA_EKPO-TXZ01.
    WA_SAIDA-WERKS    = WA_EKPO-WERKS.
    WA_SAIDA-TOT_ADTO = WL_TOT_ADTO.

    APPEND WA_SAIDA TO T_SAIDA.
    CLEAR: WA_SAIDA, WA_EKKO, WA_EKPO, WA_EKBE, WA_LFA1, WA_EKBE_AUX, WA_RBKP.
    CLEAR: WL_TOTAL_NF, WL_AWKEY, WL_TOTAL_PIS, WL_TOTAL_INSS, WL_TOTAL_IR, WG_TOTAL.
  ENDLOOP.



ENDFORM.                    " ORGANIZAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT." USING 'T_SAIDA'.
  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
  WL_LAYOUT-BOX_TABNAME  = 'T_SAIDA'.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
*      I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND' "sem 2º click
      IT_FIELDCAT             = ESTRUTURA[]
      IS_LAYOUT               = WL_LAYOUT
      I_SAVE                  = 'A'
      IT_EVENTS               = EVENTS
*      IS_LAYOUT               = LAYOUT
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = T_SAIDA.



ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.

  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.


ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.

  PERFORM MONTAR_ESTRUTURA USING:
        1  'EKPO'   'EBELN'      'T_SAIDA' 'EBELN'     ' '                   ' ' ,
        2  'EKPO'   'EBELP'      'T_SAIDA' 'EBELP'     ' '                   ' ' ,
        3  'EKKO'   'LIFNR'      'T_SAIDA' 'LIFNR'     ' '                   ' ' ,
        4  'LFA1'   'NAME1'      'T_SAIDA' 'NAME1'     'Nome'                ' ' ,
        5  'EKPO'   'TXZ01'      'T_SAIDA' 'TXZ01'     ' '                   ' ' ,
        6  'EKPO'   'WERKS'      'T_SAIDA' 'WERKS'     ' '                   ' ' ,
        7  'EKBE'   'WRBTR'      'T_SAIDA' 'TOT_ADTO'  'Vlr Adto em Aberto'  ' ' ,
        8  'RBKP'   'RMWWR'      'T_SAIDA' 'SDO_PREST' 'Sdo Prest Ctas'      ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR: WA_ESTRUTURA.


  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  IF P_FIELD EQ 'EBELN'.
    WA_ESTRUTURA-HOTSPOT = 'X'.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD..     "#EC CALLED
  DATA: VL_FORMNAME           TYPE TDSFNAME,
        VL_NAME               TYPE RS38L_FNAM,
        LS_CONTROL            TYPE SSFCTRLOP,
        LS_OPTIONS            TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO       TYPE SSFCRESCL,
        JOB_OUTPUT_OPTIONS    TYPE SSFCRESOP,
        WL_DIALOG.


  CASE SY-UCOMM.
      WHEN'&SD_ADTO'.
      VL_FORMNAME = 'ZFIR0004'.
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          FORMNAME           = VL_FORMNAME
        IMPORTING
          FM_NAME            = VL_NAME
        EXCEPTIONS
          NO_FORM            = 1
          NO_FUNCTION_MODULE = 2
          OTHERS             = 3.

      LOOP AT T_SAIDA INTO WA_SAIDA
         WHERE MARK IS NOT INITIAL.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          EXIT.
        ENDIF.

        IF WL_DIALOG EQ C_X.
          LS_CONTROL-NO_DIALOG = C_X.
*          LS_CONTROL-PREVIEW = C_X.
          MOVE-CORRESPONDING JOB_OUTPUT_OPTIONS TO LS_CONTROL.
          MOVE-CORRESPONDING JOB_OUTPUT_OPTIONS TO LS_OPTIONS.
          MOVE-CORRESPONDING JOB_OUTPUT_INFO TO LS_OPTIONS.
          MOVE JOB_OUTPUT_OPTIONS-TDPREVIEW TO LS_CONTROL-PREVIEW.

        ELSE.
          LS_CONTROL-NO_DIALOG = ' '.
*          LS_CONTROL-PREVIEW   = SPACE.
**  Impressora
*        LS_CONTROL-NO_DIALOG = ' '. "Evita la pantalla de opciones de salida del formulario
*          LS_OPTIONS-TDDEST   = ' '."'LOCL'.
*          LS_OPTIONS-TDIMMED  = ' '.
*          LS_OPTIONS-TDNEWID  = ' '.
*          LS_OPTIONS-TDNOARCH = ' '.
*
*          LS_CONTROL-DEVICE  = 'PRINTER'.
*          LS_CONTROL-GETOTF  = ' '.
        ENDIF.
        CLEAR:JOB_OUTPUT_INFO, JOB_OUTPUT_OPTIONS.

        CALL FUNCTION VL_NAME
          EXPORTING
            USER_SETTINGS      = ' '
            CONTROL_PARAMETERS = LS_CONTROL
            OUTPUT_OPTIONS     = LS_OPTIONS
            EBELN              = WA_SAIDA-EBELN
            EBELP              = WA_SAIDA-EBELP
            TOT_ADTO           = WA_SAIDA-TOT_ADTO
          IMPORTING
            JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
            JOB_OUTPUT_OPTIONS = JOB_OUTPUT_OPTIONS
          EXCEPTIONS
            FORMATTING_ERROR   = 1
            INTERNAL_ERROR     = 2
            SEND_ERROR         = 3
            USER_CANCELED      = 4
            OTHERS             = 5.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        WL_DIALOG = C_X.
      ENDLOOP.
    WHEN OTHERS.
      IF UCOMM EQ '&IC1'.
        IF SELFIELD-FIELDNAME EQ 'EBELN'.
          READ TABLE T_SAIDA INTO WA_SAIDA INDEX SELFIELD-TABINDEX.
*    Set parameter ID for transaction screen field
          SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
*    execute transaction ME23N, and skip initial data entry screen
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM. "XUSER_COMMAND


*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE

*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XPF_STATUS_SET USING UCOMM TYPE KKBLO_T_EXTAB.         "#EC CALLED

  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "XPF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.

  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

ENDFORM.                    " INICIAR_VARIAVES
