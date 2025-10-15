*----------------------------------------------------------------------*
* Report  ZFIY0031                                                     *
* Descrição  : Compensação Fatura Propria - Argentina                  *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 18/04/2013 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT  ZFIY0031 MESSAGE-ID SABAPDOCU.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON.

TABLES: KNA1,ZSDYT0049,BKPF,VBRK.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
      BEGIN OF TY_VBRK,
        FKDAT  TYPE VBRK-FKDAT,
        VKORG  TYPE VBRK-VKORG,
        VBELN  TYPE VBRK-VBELN,
        GJAHR  TYPE BKPF-GJAHR,
        AWKEY  TYPE BKPF-AWKEY,
      END OF TY_VBRK,

      BEGIN OF TY_SAIDA,
        DOC_COMP  TYPE ZSDYT0049-AUGBL,
        OBJ_KEY   TYPE ZSDYT0049-OBJ_KEY,
        KUNNR     TYPE ZSDYT0049-KUNNR,
        NAME1_C   TYPE KNA1-NAME1,
        BSTKD     TYPE ZSDYT0049-BSTKD,
        VBELN     TYPE ZSDYT0049-VBELN,
        DOC_FAT   TYPE ZSDYT0049-DOC_FAT,
        FKDAT     TYPE VBRK-FKDAT,
        NFNUM     TYPE ZSDYT0049-NFNUM,
        TIPO_MOVTO  TYPE STRING,
        VR_FATURA TYPE ZSDYT0049-VR_FATURA,
        LIFNR     TYPE ZSDYT0049-LIFNR,
        NAME1     TYPE LFA1-NAME1,
        TP_MOVTO  TYPE ZSDYT0049-TP_MOVTO,
        GSBER     TYPE VBRP-GSBER,
        BELNR     TYPE BKPF-BELNR,
        BUKRS     TYPE VBRK-BUKRS,
        GJAHR     TYPE VBRK-GJAHR,
        AUGBL     TYPE ZSDYT0049-AUGBL,
        AUGDT     TYPE ZSDYT0049-AUGDT,
        AWKEY     TYPE BKPF-AWKEY,
        ROWCOLOR(4) TYPE C,
        ENC_MANUAL  TYPE C,
        AUGBL_EM   TYPE ZSDYT0049-AUGBL,
        AUGDT_EM   TYPE ZSDYT0049-AUGDT,
      END OF TY_SAIDA.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: T_ZSDYT0049 TYPE TABLE OF ZSDYT0049,
      T_KNA1      TYPE TABLE OF KNA1,
      T_LFA1_C    TYPE TABLE OF LFA1,
      T_LFA1      TYPE TABLE OF LFA1,
      T_VBRP      TYPE TABLE OF VBRP,
      T_VBRK      TYPE TABLE OF VBRK,
      T_VBRK_AUX  TYPE TABLE OF TY_VBRK,
      T_BKPF      TYPE TABLE OF BKPF,
      T_BSID      TYPE TABLE OF BSID,
      T_BSAD      TYPE TABLE OF BSAD,
      TI_BDC      TYPE TABLE OF BDCDATA,
      TI_MSG      TYPE TABLE OF BDCMSGCOLL,
      T_SAIDA     TYPE TABLE OF TY_SAIDA,
      T_SAIDA_SELECTION TYPE TABLE OF TY_SAIDA.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: WA_ZSDYT0049        TYPE ZSDYT0049,
      WA_KNA1             TYPE KNA1,
      WA_LFA1_C           TYPE LFA1,
      WA_LFA1             TYPE LFA1,
      WA_VBRP             TYPE VBRP,
      WA_VBRK             TYPE VBRK,
      WA_VBRK_AUX         TYPE TY_VBRK,
      WA_BKPF             TYPE BKPF,
      WA_BSID             TYPE BSID,
      WA_BSAD             TYPE BSAD,
      WA_BDC              TYPE BDCDATA,
      WA_SAIDA            TYPE TY_SAIDA,
      WA_SAIDA_SELECTION TYPE TY_SAIDA.

DATA:
      WA_CONT             TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV              TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT           TYPE LVC_S_LAYO,
      WA_STABLE           TYPE LVC_S_STBL.

*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA: IT_FCAT             TYPE TABLE OF LVC_S_FCAT,
      GS_ALV_REFRES_COND  TYPE LVC_S_STBL,
      FORMAPGTO           TYPE ZIB_CONTABIL-ZLSCH,
      BCOEMPRESA          TYPE ZIB_CONTABIL-HBKID,
      DTVENCTO            TYPE ZIB_CONTABIL-ZFBDT,
      PROC_V              TYPE C LENGTH 1,
      GS_VARIANT_C        TYPE DISVARIANT.

*----------------------------------------------------------------------*
***INCLUDE ZFIR0060_0001 .                                             *
*----------------------------------------------------------------------*

DATA: DG_DYNDOC_ID     TYPE REF TO CL_DD_DOCUMENT.

DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
        LINE(255) TYPE X,
      END OF GRAPHIC_TABLE.

DATA: L_GRAPHIC_XSTR TYPE XSTRING.
DATA: GRAPHIC_SIZE   TYPE I.
DATA: L_GRAPHIC_CONV TYPE I.
DATA: L_GRAPHIC_OFFS TYPE I.

*---------- Definition -----------------------------------------------*
*CLASS LCL_EVENT_HANDLER DEFINITION.
*  PUBLIC SECTION.
*    METHODS HANDLE_HOTSPOT_CLICK
*      FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*      IMPORTING E_ROW_ID
*                E_COLUMN_ID
*                ES_ROW_NO.
**    METHODS TOP_OF_PAGE
**      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
**      IMPORTING E_DYNDOC_ID.
*ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Inclementação  -------------------------------------------*
*CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
*  METHOD HANDLE_HOTSPOT_CLICK.
*
*  ENDMETHOD.                    "handle_hotspot_click
*ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: PICTURE          TYPE REF TO CL_GUI_PICTURE,
      GF_FIRST_DISPLAY TYPE C VALUE 'X',
      CTL_CCCONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_2    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_HTML   TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_HTML1  TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_HTML2  TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_GRID   TYPE REF TO CL_GUI_CONTAINER,
*      EVENT_HANDLER    TYPE REF TO LCL_EVENT_HANDLER,
      DG_HTML_CNTRL    TYPE REF TO CL_GUI_HTML_VIEWER,
*      CTL_ALV_RESUMO   TYPE REF TO CL_GUI_ALV_GRID,
      GS_SCROLL_COL    TYPE LVC_S_COL,
      GS_SCROLL_ROW    TYPE LVC_S_ROID,
      GS_LAYOUT        TYPE LVC_S_LAYO,
      IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS.




*&---------------------------------------------------------------------*
* ALV selection
*&---------------------------------------------------------------------*
DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
      WA_SELECTED_ROWS TYPE LVC_S_ROW.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

*SELECT-OPTIONS: p_kunnr FOR kna1-kunnr.
SELECT-OPTIONS: P_KUNNR FOR ZSDYT0049-KUNNR,
                P_LIFNR FOR ZSDYT0049-LIFNR,
                P_FKDAT FOR VBRK-FKDAT.

SELECTION-SCREEN: END OF BLOCK B1.

" Seleção de Campos (RadioButton)
SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-016.
PARAMETERS:     FC_COMP    TYPE CHAR1  RADIOBUTTON GROUP RB01  USER-COMMAND MUDA_TELA , " Compensadas
                FC_ACOMP   TYPE CHAR1  RADIOBUTTON GROUP RB01 . " A Compensar

PARAMETER:P_BUDAT TYPE BKPF-BUDAT.

SELECT-OPTIONS: P_AUGDT FOR ZSDYT0049-AUGDT.

SELECTION-SCREEN: END OF BLOCK B3.

INITIALIZATION.
  FC_ACOMP = ABAP_TRUE.
*=============================================================================*
*AT SELECTION-SCREEN OUTPUT                                                   *
*=============================================================================*

AT SELECTION-SCREEN OUTPUT.

*  IF  FC_ACOMP = 'X' .
*    MESSAGE 'Informe a Data de Lançamento.' TYPE 'I'.
*  ENDIF.

  LOOP AT SCREEN.
    IF  FC_COMP = 'X' .
      IF  SCREEN-NAME CS 'P_BUDAT' .
        IF FC_ACOMP IS NOT INITIAL.
          SCREEN-INVISIBLE = 0.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ELSE.
          SCREEN-INVISIBLE = 1.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ELSE.
      IF  SCREEN-NAME CS 'P_BUDAT'   AND  SCREEN-NAME CS 'FC_ACOMP'    .
        IF FC_COMP IS NOT INITIAL.
          SCREEN-INVISIBLE = 0.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ELSE.
          SCREEN-INVISIBLE = 1.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF SCREEN-NAME CS 'P_AUGDT'.
        SCREEN-INVISIBLE = 1.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:  F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_ALV. " Form ALV

  CALL SCREEN 0100.


*AT SELECTION-SCREEN ON P_BUDAT.
*  IF FC_ACOMP = 'X'.
*    IF P_BUDAT IS INITIAL.
*      MESSAGE 'Informe a Data de Lançamento' TYPE 'E'.
*    ENDIF.
*  ENDIF.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

  DATA: VL_LEN    TYPE I,
        VL_AWKEY  TYPE BKPF-AWKEY.

  REFRESH: T_ZSDYT0049, T_LFA1, T_KNA1, T_LFA1_C, T_VBRP, T_VBRK, T_BKPF, T_BSID,T_BSAD.


  IF FC_COMP EQ 'X'.

    SELECT A~OBJ_KEY A~INTERFACE A~KUNNR A~BSTKD A~WERKS A~COD_MVTO
           A~TP_MOVTO A~VL_FATURA A~RG_ATUALIZADO A~VBELN A~DOC_FAT
           A~NFNUM A~LIFNR A~VR_FATURA A~AUGBL A~AUGDT A~VR_BASE_IVA
           A~RG_PDF_FATURA A~RG_CTB_FATURA A~CTB_BUKRS A~CTB_BELNR
           A~CTB_GJAHR
      FROM ZSDYT0049 AS A
      INNER JOIN VBRK ON VBRK~VBELN  = A~DOC_FAT "#EC CI_DB_OPERATION_OK[2768887]
      INTO CORRESPONDING FIELDS OF TABLE  T_ZSDYT0049
     WHERE A~AUGBL NE SPACE
       AND A~AUGDT IN P_AUGDT
       AND A~KUNNR IN P_KUNNR
       AND VBRK~FKDAT IN P_FKDAT AND VBRK~DRAFT = SPACE .

  ELSEIF FC_ACOMP EQ 'X'.

    IF P_BUDAT IS INITIAL.
      MESSAGE 'Informe a Data a compensar' TYPE 'I'.
      STOP.
    ENDIF.

    SELECT A~OBJ_KEY A~INTERFACE A~KUNNR A~BSTKD A~WERKS A~COD_MVTO
           A~TP_MOVTO A~VL_FATURA A~RG_ATUALIZADO A~VBELN A~DOC_FAT
           A~NFNUM A~LIFNR A~VR_FATURA A~AUGBL A~AUGDT A~VR_BASE_IVA
           A~RG_PDF_FATURA A~RG_CTB_FATURA A~CTB_BUKRS A~CTB_BELNR
           A~CTB_GJAHR
      FROM ZSDYT0049 AS A
      INNER JOIN VBRK ON  VBRK~VBELN  = A~DOC_FAT "#EC CI_DB_OPERATION_OK[2768887]
      INTO CORRESPONDING FIELDS OF TABLE T_ZSDYT0049
     WHERE A~AUGBL EQ SPACE
       AND A~KUNNR IN P_KUNNR
       AND VBRK~FKDAT IN P_FKDAT AND VBRK~DRAFT = SPACE .

*    SELECT *
*      FROM ZSDYT0049
*      INTO TABLE T_ZSDYT0049
*     WHERE AUGBL EQ SPACE
*       AND KUNNR IN P_KUNNR.

*     SELECT *
  ENDIF.

  IF SY-SUBRC IS INITIAL.

    SELECT *
      FROM LFA1
      INTO TABLE T_LFA1
       FOR ALL ENTRIES IN T_ZSDYT0049
     WHERE LIFNR = T_ZSDYT0049-LIFNR.

    SELECT *
      FROM  KNA1
      INTO TABLE T_KNA1
       FOR ALL ENTRIES IN T_ZSDYT0049
     WHERE KUNNR = T_ZSDYT0049-KUNNR.

    SELECT *
      FROM LFA1
      INTO TABLE T_LFA1_C
       FOR ALL ENTRIES IN T_KNA1
     WHERE LIFNR = T_KNA1-LIFNR.

    SELECT *
      FROM VBRP
      INTO TABLE T_VBRP
        FOR ALL ENTRIES IN T_ZSDYT0049
      WHERE VBELN	=	T_ZSDYT0049-DOC_FAT.

    SELECT *
      FROM VBRK
      INTO TABLE T_VBRK
       FOR ALL ENTRIES IN T_ZSDYT0049
     WHERE VBELN  = T_ZSDYT0049-DOC_FAT.


    LOOP AT T_VBRK INTO WA_VBRK.

      CLEAR : WA_VBRK_AUX.

      WA_VBRK_AUX-FKDAT = WA_VBRK-FKDAT.
      WA_VBRK_AUX-VKORG = WA_VBRK-VKORG.
      WA_VBRK_AUX-VBELN = WA_VBRK-VBELN.
      WA_VBRK_AUX-GJAHR = WA_VBRK-FKDAT(4).


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_VBRK-VBELN
        IMPORTING
          OUTPUT = VL_AWKEY.

      WA_VBRK_AUX-AWKEY = VL_AWKEY+10(10).

      APPEND WA_VBRK_AUX TO T_VBRK_AUX.
    ENDLOOP.

    SELECT *
      FROM BKPF
      INTO TABLE T_BKPF
      FOR ALL ENTRIES IN T_VBRK_AUX
     WHERE BUKRS =  T_VBRK_AUX-VKORG
       AND GJAHR =  T_VBRK_AUX-GJAHR
       AND AWKEY =  T_VBRK_AUX-AWKEY.

    IF T_BKPF[] IS NOT INITIAL.
      SELECT *
        FROM BSID
        INTO TABLE T_BSID
        FOR ALL ENTRIES IN T_BKPF
        WHERE BUKRS =  T_BKPF-BUKRS
          AND GJAHR =  T_BKPF-GJAHR
          AND BELNR =  T_BKPF-BELNR.

      SELECT *
        FROM BSAD
        INTO TABLE T_BSAD
        FOR ALL ENTRIES IN T_BKPF
        WHERE BUKRS =  T_BKPF-BUKRS
          AND GJAHR =  T_BKPF-GJAHR
          AND BELNR =  T_BKPF-BELNR.

    ENDIF.


  ENDIF.

ENDFORM.                    "F_SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  SORT : T_KNA1   BY KUNNR,
         T_VBRK   BY VBELN,
         T_VBRK_AUX BY VBELN,
         T_VBRP   BY VBELN,
         T_LFA1_C BY LIFNR,
         T_LFA1   BY LIFNR,
         T_BKPF   BY BUKRS GJAHR AWKEY,
         T_BSID   BY BUKRS GJAHR BELNR,
         T_BSAD   BY BUKRS GJAHR BELNR.

  REFRESH T_SAIDA.

  LOOP AT T_ZSDYT0049 INTO WA_ZSDYT0049.

    CLEAR:  WA_KNA1, WA_LFA1_C, WA_LFA1, WA_VBRP, WA_VBRK, WA_VBRK_AUX, WA_BKPF, WA_BSID, WA_SAIDA.

    READ TABLE T_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZSDYT0049-KUNNR BINARY SEARCH.

    READ TABLE T_VBRP INTO WA_VBRP WITH KEY VBELN = WA_ZSDYT0049-DOC_FAT BINARY SEARCH.

    READ TABLE T_VBRK_AUX INTO WA_VBRK_AUX WITH KEY VBELN = WA_ZSDYT0049-DOC_FAT BINARY SEARCH.

    IF SY-SUBRC = 0.

      READ TABLE T_BKPF INTO WA_BKPF WITH KEY BUKRS =  WA_VBRK_AUX-VKORG
                                              GJAHR =  WA_VBRK_AUX-GJAHR
                                              AWKEY =  WA_VBRK_AUX-AWKEY BINARY SEARCH.

      IF SY-SUBRC = 0.
        CLEAR: WA_BSID.
        READ TABLE T_BSID INTO WA_BSID WITH KEY BUKRS = WA_BKPF-BUKRS
                                                GJAHR = WA_BKPF-GJAHR
                                                BELNR = WA_BKPF-BELNR BINARY SEARCH.

        IF SY-SUBRC = 0.
          WA_SAIDA-VR_FATURA = WA_BSID-DMBTR.
        ELSE.
          CLEAR: WA_BSAD.
          READ TABLE T_BSAD INTO WA_BSAD WITH KEY BUKRS = WA_BKPF-BUKRS
                                                  GJAHR = WA_BKPF-GJAHR
                                                  BELNR = WA_BKPF-BELNR BINARY SEARCH.
          IF SY-SUBRC = 0.
            IF FC_ACOMP EQ 'X'.
              WA_SAIDA-ROWCOLOR   = 'C600'.
              WA_SAIDA-ENC_MANUAL = 'X'.
            ENDIF.
            WA_SAIDA-VR_FATURA  = WA_BSAD-DMBTR.
            WA_SAIDA-AUGDT_EM   = WA_BSAD-AUGDT.
            WA_SAIDA-AUGBL_EM   = WA_BSAD-AUGBL.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.
    "WA_SAIDA-VR_FATURA = WA_ZSDYT0049-VR_FATURA.


    IF WA_ZSDYT0049-AUGBL IS INITIAL .
      WA_SAIDA-DOC_COMP  =  ICON_MESSAGE_WARNING.
    ELSE.
      WA_SAIDA-DOC_COMP  = WA_ZSDYT0049-AUGBL.
    ENDIF.

    WA_SAIDA-AWKEY     = WA_VBRK_AUX-AWKEY.
    WA_SAIDA-OBJ_KEY   = WA_ZSDYT0049-OBJ_KEY.
    WA_SAIDA-BUKRS     = WA_VBRK_AUX-VKORG.
    WA_SAIDA-GJAHR     = WA_VBRK_AUX-GJAHR.
    WA_SAIDA-KUNNR     = WA_ZSDYT0049-KUNNR.
    WA_SAIDA-NAME1_C   = WA_KNA1-NAME1.
    WA_SAIDA-BSTKD     = WA_ZSDYT0049-BSTKD.
    WA_SAIDA-VBELN     = WA_ZSDYT0049-VBELN.
    WA_SAIDA-DOC_FAT   = WA_ZSDYT0049-DOC_FAT.
    WA_SAIDA-FKDAT     = WA_VBRK_AUX-FKDAT .
    WA_SAIDA-NFNUM     = WA_ZSDYT0049-NFNUM.
    WA_SAIDA-GSBER     = WA_VBRP-GSBER.
    WA_SAIDA-BELNR     = WA_BKPF-BELNR.
    WA_SAIDA-AUGBL     = WA_ZSDYT0049-AUGBL.
    WA_SAIDA-AUGDT     = WA_ZSDYT0049-AUGDT.
    WA_SAIDA-TP_MOVTO  = WA_ZSDYT0049-TP_MOVTO.

    IF WA_ZSDYT0049-TP_MOVTO = 'D'.
      WA_SAIDA-TIPO_MOVTO  = 'Nota Débito'.
    ELSE.
      WA_SAIDA-TIPO_MOVTO  = 'Nota Crédito'.
    ENDIF.




    IF WA_ZSDYT0049-LIFNR IS NOT INITIAL.

      READ TABLE T_LFA1 INTO WA_LFA1  WITH KEY LIFNR = WA_ZSDYT0049-LIFNR BINARY SEARCH.

      WA_SAIDA-LIFNR = WA_ZSDYT0049-LIFNR.
      WA_SAIDA-NAME1 = WA_LFA1-NAME1.

    ELSE.
      READ TABLE T_LFA1_C INTO WA_LFA1_C  WITH KEY LIFNR = WA_KNA1-LIFNR BINARY SEARCH.

      WA_SAIDA-LIFNR = WA_LFA1_C-LIFNR.
      WA_SAIDA-NAME1 = WA_LFA1_C-NAME1.

    ENDIF.

    IF P_LIFNR IS NOT INITIAL.
      IF WA_SAIDA-LIFNR NOT IN P_LIFNR.
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND WA_SAIDA TO T_SAIDA.

    CLEAR : WA_SAIDA, WA_ZSDYT0049.

  ENDLOOP.

ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV.
  PERFORM ALV_PREENCHE_CAT USING:
      'DOC_COMP'      TEXT-002     '11'       'X'  'X'     ' ' ,
      'OBJ_KEY'       TEXT-003     '12'       ' '  'X'     ' ' ,
      'KUNNR'         TEXT-004     '10'       'X'  'X'     ' ' ,
      'NAME1_C'       TEXT-005     '25'       ' '  'X'     ' ' ,
      'BSTKD'         TEXT-006     '12'       ' '  'X'     ' ' ,
      'VBELN'         TEXT-007     '11'       'X'  'X'     ' ' ,
      'DOC_FAT'       TEXT-008     '11'       'X'  'X'     ' ' ,
      'FKDAT'         TEXT-009     '12'       ' '  'X'     ' ' ,
      'NFNUM'         TEXT-010     '13'       ' '  ' '     ' ' ,
      'TIPO_MOVTO'    TEXT-011     '11'       ' '  'X'     ' ' ,
      'VR_FATURA'     TEXT-012     '12'       ' '  'X'     ' ' ,
      'LIFNR'         TEXT-013     '10'       'X'  'X'     ' ' ,
      'NAME1'         TEXT-014     '25'       ' '  'X'     ' ' .


ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_STATUS OUTPUT.
  DATA: WA_FCODE TYPE SY-UCOMM,
        IT_FCODE LIKE TABLE OF WA_FCODE.

  REFRESH IT_FCODE.

  IF FC_COMP EQ 'X'.
    WA_FCODE = 'COMP'.
    APPEND WA_FCODE TO IT_FCODE.

    WA_FCODE = 'ENC_MANUAL'.
    APPEND WA_FCODE TO IT_FCODE.
  ELSE.
    WA_FCODE = 'ESTORNAR'.
    APPEND WA_FCODE TO IT_FCODE.
  ENDIF.

  SET PF-STATUS 'FF0100' EXCLUDING IT_FCODE.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZCOMPENSAR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM ZCOMPENSAR.

  DATA: VL_DATA     TYPE STRING,
        VL_DATA_C   TYPE STRING,
        VL_MES      TYPE STRING,
        VL_TP_MOVTO TYPE C LENGTH 2,
        VL_MODE     TYPE C LENGTH 1,
        VL_AUGDT    TYPE BSAD-AUGDT,
        VL_AUGBL    TYPE BSAD-AUGBL,
        VL_VRFATURA TYPE STRING ,
        E_STATUS(1),
        E_MESSA(64).

  IF T_SAIDA_SELECTION[] IS NOT INITIAL.
    READ TABLE T_SAIDA_SELECTION INTO WA_SAIDA_SELECTION INDEX 1.
    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        I_BUKRS  = WA_SAIDA_SELECTION-BUKRS
        I_DATA   = P_BUDAT
      IMPORTING
        E_STATUS = E_STATUS
        E_MESSA  = E_MESSA
      EXCEPTIONS
        ERROR    = 1
        OTHERS   = 2.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF  E_STATUS = 'E'.
      MESSAGE E_MESSA TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.


  VL_MODE = 'N'.
*  VL_DATA = SY-DATUM.
  VL_DATA = P_BUDAT.
  "concatenate SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into VL_DATA.

  CONCATENATE VL_DATA+6(2) '.' VL_DATA+4(2) '.' VL_DATA(4) INTO VL_DATA_C.

  VL_MES = VL_DATA+4(2).

*  CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) INTO VL_DATA.
  CONCATENATE P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT(4) INTO VL_DATA.

  LOOP AT T_SAIDA_SELECTION INTO WA_SAIDA_SELECTION.

    IF WA_SAIDA_SELECTION-TP_MOVTO = 'D'.
      VL_TP_MOVTO = '21'.
    ELSE.
      VL_TP_MOVTO = '31'.
    ENDIF.
    IF  WA_SAIDA_SELECTION-VR_FATURA <= 0.
      MESSAGE 'O Montante não pode ser 0 !'  TYPE 'W'.
      STOP.
    ENDIF.

    VL_VRFATURA = WA_SAIDA_SELECTION-VR_FATURA.

    CONDENSE VL_VRFATURA NO-GAPS.

    REPLACE '.' WITH ',' INTO VL_VRFATURA.

    REFRESH TI_BDC.

    PERFORM ZF_BDC USING: 'X' 'SAPMF05A'        '0122',
                          ' ' 'BDC_CURSOR'      'RF05A-NEWKO',
                          ' ' 'BDC_OKCODE'      '/00',
                          ' ' 'BKPF-BLDAT'       VL_DATA,
                          ' ' 'BKPF-BLART'      'SA',
                          ' ' 'BKPF-BUKRS'      '0100',
                          ' ' 'BKPF-BUDAT'      VL_DATA_C ,
                          ' ' 'BKPF-MONAT'      VL_MES,
                          ' ' 'BKPF-WAERS'      'ARS',
                          ' ' 'BKPF-XBLNR'      WA_SAIDA_SELECTION-NFNUM,
                          ' ' 'RF05A-NEWBS'     VL_TP_MOVTO,
                          ' ' 'RF05A-NEWKO'     WA_SAIDA_SELECTION-LIFNR,

                          'X' 'SAPMF05A'        '0302',
                          ' ' 'BDC_CURSOR'      'BSEG-GSBER',
                          ' ' 'BDC_OKCODE'      '=PA',
                          ' ' 'BSEG-WRBTR'      VL_VRFATURA,
                          ' ' 'BSEG-GSBER'      WA_SAIDA_SELECTION-GSBER,
                          ' ' 'BSEG-ZFBDT'      VL_DATA_C,

                          'X' 'SAPMF05A'        '0710',
                          ' ' 'BDC_CURSOR'      'RF05A-XPOS1(03)',
                          ' ' 'BDC_OKCODE'      '/00',
                          ' ' 'RF05A-AGBUK'	    '0100',
                          ' ' 'RF05A-AGKON'	    WA_SAIDA_SELECTION-KUNNR,
                          ' ' 'RF05A-AGKOA'    	'D',
                          ' ' 'RF05A-XNOPS'	    'X',
                          ' ' 'RF05A-XPOS1(01)' ' ',
                          ' ' 'RF05A-XPOS1(03)'	'X',

                          'X' 'SAPMF05A'        '0731',
                          ' ' 'BDC_CURSOR'      'RF05A-SEL01(01)',
                          ' ' 'BDC_OKCODE'      '=PA',
                          ' ' 'RF05A-SEL01(01)' WA_SAIDA_SELECTION-BELNR,
*                          'X' 'SAPDF05X'        '3100',
*                          ' ' 'BDC_OKCODE'      '=PI',
*                          ' ' 'BDC_SUBSCR'      'SAPDF05X                                6102PAGE',                 "               6102PAGE
*                          ' ' 'BDC_CURSOR'      'DF05B-PSBET(01)',
*                          ' ' 'RF05A-ABPOS'      '1',
                          'X' 'SAPDF05X'        '3100',
                          ' ' 'BDC_OKCODE'      '=BU',
                          ' ' 'BDC_SUBSCR'      'SAPDF05X                                6102PAGE',              "                  6102PAGE
                          ' ' 'BDC_CURSOR'      'DF05B-PSBET(01)',
                          ' ' 'RF05A-ABPOS'	    '1'.


    CALL TRANSACTION 'FB05'
         USING TI_BDC
          MODE VL_MODE
        UPDATE 'S'
      MESSAGES INTO TI_MSG.

    COMMIT WORK.

    SELECT SINGLE AUGDT AUGBL
      INTO (VL_AUGDT, VL_AUGBL)
      FROM BSAD
     WHERE BELNR = WA_SAIDA_SELECTION-BELNR
       AND BUKRS = WA_SAIDA_SELECTION-BUKRS
       AND GJAHR = WA_SAIDA_SELECTION-GJAHR.

    IF SY-SUBRC IS INITIAL.
      UPDATE ZSDYT0049
         SET AUGDT = VL_AUGDT
             AUGBL = VL_AUGBL
       WHERE OBJ_KEY = WA_SAIDA_SELECTION-OBJ_KEY.
      COMMIT WORK.
    ELSE.
      CONCATENATE TEXT-017 WA_SAIDA_SELECTION-OBJ_KEY INTO E_MESSA SEPARATED BY SPACE.
      MESSAGE E_MESSA TYPE 'I'.
    ENDIF.

  ENDLOOP.

  PERFORM ZRENOVAR.

ENDFORM.                    "ZCOMPENSAR
*&---------------------------------------------------------------------*
*&      Module  ZCOMPENSAR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZESTORNAR.
  DATA: VL_MODE     TYPE C LENGTH 1,
        VL_ANO      TYPE C LENGTH 4,
        VL_MES      TYPE C LENGTH 4,
        VL_STBLG    TYPE BKPF-STBLG.

  VL_MODE = 'N'.



  LOOP AT T_SAIDA_SELECTION INTO WA_SAIDA_SELECTION.

    REFRESH TI_BDC.

    VL_ANO = WA_SAIDA_SELECTION-AUGDT(4).
    VL_MES = WA_SAIDA_SELECTION-AUGDT+4(2).

    PERFORM ZF_BDC USING: 'X' 'SAPMF05R'    '0100',
                          ' ' 'BDC_CURSOR'  'RF05R-AUGBL',
                          ' ' 'BDC_OKCODE'  '=RAGL',
                          ' ' 'RF05R-AUGBL'	WA_SAIDA_SELECTION-AUGBL," (ZSDYT0049-AUGBL)
                          ' ' 'RF05R-BUKRS'	'0100',
                          ' ' 'RF05R-GJAHR'	VL_ANO, "(Ano campo ZSDTY0049-AUGDT ou data informada no parâmetro Data do Estorno)

                          'X' 'SAPLSPO2'   	'0100',
                          ' ' 'BDC_OKCODE'  '=OPT2',

                          'X' 'SAPMF05R'    '0300',
                          ' ' 'BDC_CURSOR'  'RF05R-MONAT',
                          ' ' 'BDC_OKCODE'  '=ENTR',
                          ' ' 'RF05R-STGRD'	'01',
                          ' ' 'RF05R-BUDAT'	WA_SAIDA_SELECTION-AUGBL," (ZSDTY0049-AUGDT ou data informada no parâmetro Data do Estorno)
                          ' ' 'RF05R-MONAT'	VL_MES,"(Mês ZSDTY0049-AUGDT ou data informada no parâmetro Data do Estorno)

                          'X' 'SAPMF05R'    '0100',
                          ' ' 'BDC_CURSOR'  'RF05R-AUGBL',
                          ' ' 'BDC_OKCODE'  '=EZUR',
                          ' ' 'RF05R-BUKRS'	'0100',
                          ' ' 'RF05R-GJAHR'	VL_ANO ,"(Ano campo ZSDTY0049-AUGDT ou data informada no parâmetro Data do Estorno)

                          'X' 'SAPMF05R'   	'0100',
                          ' ' 'BDC_OKCODE'  '/EEEND',
                          ' ' 'BDC_CURSOR'  'RF05R-AUGBL'.


    CALL TRANSACTION 'FBRA'
         USING TI_BDC
          MODE VL_MODE
        UPDATE 'S'
      MESSAGES INTO TI_MSG.

    COMMIT WORK.

    REFRESH TI_BDC.

    PERFORM ZF_BDC USING: 'X' 'SAPMF05A'    '0105',
                          ' ' 'BDC_CURSOR'  'UF05A-STGRD',
                          ' ' 'BDC_OKCODE'  '=BU',
                          ' ' 'RF05A-BELNS'	WA_SAIDA_SELECTION-AUGBL,
                          ' ' 'BKPF-BUKRS'  WA_SAIDA_SELECTION-BUKRS,
                          ' ' 'RF05A-GJAHS'	VL_ANO,
                          ' ' 'UF05A-STGRD'	'01'.

    CALL TRANSACTION 'FB08'
         USING TI_BDC
          MODE VL_MODE
        UPDATE 'S'
      MESSAGES INTO TI_MSG.

    COMMIT WORK.

    SELECT SINGLE STBLG
      INTO VL_STBLG
      FROM BKPF
     WHERE BELNR = WA_SAIDA_SELECTION-AUGBL
       AND GJAHR = VL_ANO
       AND BUKRS = WA_SAIDA_SELECTION-BUKRS .

    IF VL_STBLG IS NOT INITIAL.

      UPDATE ZSDYT0049
           SET AUGDT = ''
               AUGBL = ''
         WHERE OBJ_KEY = WA_SAIDA_SELECTION-OBJ_KEY.

      COMMIT WORK.

    ENDIF.

  ENDLOOP.

  PERFORM ZRENOVAR.

ENDFORM.                    "zestornar

*&---------------------------------------------------------------------*
*&      Form  ZRENOVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM ZRENOVAR .

  PERFORM:  F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA.

  IF WA_ALV IS NOT INITIAL.
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    REFRESH: IT_SELECTED_ROWS.

  ENDIF.

ENDFORM.                    "ZRENOVAR
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO TYPE C
                               P_DESC  TYPE C
                               P_TAM   TYPE C
                               P_HOT   TYPE C
                               P_ZERO  TYPE C
                               P_MASK  TYPE C.
  DATA: WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-TABNAME   = 'T_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-EDIT_MASK = P_MASK.
  WL_FCAT-OUTPUTLEN = P_TAM.

  "wl_fcat-convexit  = p_mask.
  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_PREENCHE_CAT

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA: WA_EVENT       TYPE REF TO  LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
            ZM_HANDLE_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID
                      E_COLUMN_ID
                      ES_ROW_NO,

            ZM_HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING
                E_OBJECT E_INTERACTIVE,

            ZM_HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING
                 E_UCOMM.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT.
    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
                                      E_COLUMN_ID
                                      ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD ZM_HANDLE_TOOLBAR.
*   Incluindo Botão ALV
    PERFORM Z_HANDLE_TOOLBAR USING E_OBJECT
                                   E_INTERACTIVE.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD ZM_HANDLE_USER_COMMAND.
*   User Command Botões Incluidos
    PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT                                 *
*&---------------------------------------------------------------------*
MODULE CREATE_OBJECTS OUTPUT.

  DATA: URL(255) TYPE C.


*   Create object for container
  IF WA_CONT IS INITIAL.
    CREATE OBJECT WA_CONT
      EXPORTING
        CONTAINER_NAME              = 'CC_ALV'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.


    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = WA_CONT
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_HTML.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_HTML
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_HTML1.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 30.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = DG_PARENT_HTML2.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_HTML2.

    PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_GRID.

    CALL METHOD DG_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 15.


*   Create object for ALV grid inside container
    CREATE OBJECT WA_ALV
      EXPORTING
        I_PARENT = DG_PARENT_GRID.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.
  ENDIF.

*  IF WA_ALV IS INITIAL AND NOT
*     WA_CONT IS INITIAL.
*
*    CREATE OBJECT WA_ALV
*      EXPORTING
*        I_PARENT          = WA_CONT
*      EXCEPTIONS
*        ERROR_CNTL_CREATE = 1
*        ERROR_CNTL_INIT   = 2
*        ERROR_CNTL_LINK   = 3
*        ERROR_DP_CREATE   = 4
*        OTHERS            = 5.
*  ENDIF.

  IF WA_EVENT IS INITIAL.

    CREATE OBJECT WA_EVENT.
    SET HANDLER: WA_EVENT->ZM_HANDLE_HOTSPOT FOR WA_ALV.
    SET HANDLER: WA_EVENT->ZM_HANDLE_TOOLBAR FOR WA_ALV.
    SET HANDLER: WA_EVENT->ZM_HANDLE_USER_COMMAND FOR WA_ALV.

  ENDIF.

  WA_LAYOUT-SEL_MODE = 'A'.
  WA_LAYOUT-INFO_FNAME = 'ROWCOLOR'.

  PERFORM CRIA_HTML_CAB.

**   Send data to ALV grid
  CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IS_VARIANT                    = GS_VARIANT_C
      I_SAVE                        = 'A'
      IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE_FCODE
    CHANGING
      IT_FIELDCATALOG               = IT_FCAT
      IT_OUTTAB                     = T_SAIDA[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CHECK NOT WA_ALV IS INITIAL.


*  CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

*  CALL METHOD WA_ALV->SET_SCROLL_INFO_VIA_ID
*    EXPORTING
*      IS_COL_INFO = GS_SCROLL_COL
*      IS_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

**&---------------------------------------------------------------------*
**&      Module  Z_EXIBE_ALV  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE Z_EXIBE_ALV OUTPUT.

*  IF WA_CONT IS INITIAL.
*
*
*    CREATE OBJECT WA_CONT
*      EXPORTING
*        CONTAINER_NAME              = 'CC_ALV'
*      EXCEPTIONS
*        CNTL_ERROR                  = 1
*        CNTL_SYSTEM_ERROR           = 2
*        CREATE_ERROR                = 3
*        LIFETIME_ERROR              = 4
*        LIFETIME_DYNPRO_DYNPRO_LINK = 5
*        OTHERS                      = 6.
*  ENDIF.
*  IF WA_ALV IS INITIAL AND NOT
*    WA_CONT IS INITIAL.
*
*    CREATE OBJECT WA_ALV
*      EXPORTING
*        I_PARENT          = WA_CONT
*      EXCEPTIONS
*        ERROR_CNTL_CREATE = 1
*        ERROR_CNTL_INIT   = 2
*        ERROR_CNTL_LINK   = 3
*        ERROR_DP_CREATE   = 4
*        OTHERS            = 5.
*  ENDIF.
*
*  IF WA_EVENT IS INITIAL.
*
*    CREATE OBJECT WA_EVENT.
*    SET HANDLER: WA_EVENT->ZM_HANDLE_HOTSPOT FOR WA_ALV.
*    SET HANDLER: WA_EVENT->ZM_HANDLE_TOOLBAR FOR WA_ALV.
*    SET HANDLER: WA_EVENT->ZM_HANDLE_USER_COMMAND FOR WA_ALV.
*
*  ENDIF.
*
*  WA_LAYOUT-SEL_MODE = 'A'.
*
*  CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
*    EXPORTING
*      IS_LAYOUT                     = WA_LAYOUT
*      IS_VARIANT                    = GS_VARIANT_C
*      I_SAVE                        = 'A'
*    CHANGING
*      IT_OUTTAB                     = T_SAIDA
*      IT_FIELDCATALOG               = IT_FCAT
*    EXCEPTIONS
*      INVALID_PARAMETER_COMBINATION = 1
*      PROGRAM_ERROR                 = 2
*      TOO_MANY_LINES                = 3
*      OTHERS                        = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  CHECK NOT WA_ALV IS INITIAL.
*ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_USER_COMMAND INPUT.
  IF SY-DYNNR EQ '0100'.
    CASE SY-UCOMM.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
      WHEN 'COMP' .
        PERFORM ZCOMPENSAR.
      WHEN 'ESTORNAR'.
        PERFORM ZESTORNAR.
      WHEN 'ENC_MANUAL'.
        PERFORM ZENC_MANUAL.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM Z_HANDLE_HOTSPOT  USING    P_E_ROW_ID    TYPE LVC_S_ROW
                                P_E_COLUMN_ID TYPE  LVC_S_COL
                                P_ES_ROW_NO   TYPE  LVC_S_ROID.
  DATA OPT TYPE CTU_PARAMS.

  CASE P_E_COLUMN_ID.
    WHEN 'DOC_COMP'.
      READ TABLE T_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
      IF WA_SAIDA-DOC_COMP IS NOT INITIAL.
        SET PARAMETER ID  'BLN' FIELD WA_SAIDA-DOC_COMP.
        SET PARAMETER ID  'BUK' FIELD WA_SAIDA-BUKRS.
        SET PARAMETER ID  'GJR' FIELD WA_SAIDA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'KUNNR'.
      READ TABLE T_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
      IF WA_SAIDA-KUNNR IS NOT INITIAL.

*---> 06/07/2023 - Migração S4 - DG
*        SET PARAMETER ID  'KUN' FIELD WA_SAIDA-KUNNR.
*        CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN.
    DATA: lo_migbp TYPE REF TO /mignow/cl_migbp,
          LV_kunnr TYPE kunnr.

     LV_kunnr = CONV #( WA_SAIDA-KUNNR ).

    Create OBJECT LO_MIGBP
      EXPORTING
        IM_TEST  = ABAP_false
        IM_TCODE = 'BP' .
      .
    CALL METHOD lo_migbp->MT_BP_DISPLAY
      EXPORTING
*        IM_PARTNER       =
        IM_LIFNR         = space
        IM_KUNNR         = lv_kunnr
        IM_VENDOR_VIEW   = space
        IM_CUSTOMER_VIEW = space.
*      IMPORTING
*        EM_ERRO          =
      .
*<--- 06/07/2023 - Migração S4 - DG
      ENDIF.

    WHEN 'VBELN'.
      READ TABLE T_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
      IF WA_SAIDA-VBELN IS NOT INITIAL.
        SET PARAMETER ID  'AUN' FIELD WA_SAIDA-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'DOC_FAT'.
      READ TABLE T_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
      IF WA_SAIDA-DOC_FAT IS NOT INITIAL.
        SET PARAMETER ID  'VF' FIELD WA_SAIDA-DOC_FAT.
        CALL TRANSACTION  'VF03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'LIFNR'.
      READ TABLE T_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.
      IF WA_SAIDA-LIFNR IS NOT INITIAL.

*---> 06/07/2023 - Migração S4 - DG
*        SET PARAMETER ID  'LIF' FIELD WA_SAIDA-LIFNR.
*        SET PARAMETER ID  'BUK' FIELD WA_SAIDA-BUKRS.
*        CALL TRANSACTION  'XK03' AND SKIP FIRST SCREEN.
    DATA: LV_LIFNR TYPE LIFNR.

     LV_LIFNR = CONV #( WA_SAIDA-LIFNR ).

    Create OBJECT LO_MIGBP
      EXPORTING
        IM_TEST  = ABAP_false
        IM_TCODE = 'BP' .
      .
    CALL METHOD lo_migbp->MT_BP_DISPLAY
      EXPORTING
*        IM_PARTNER       =
        IM_LIFNR         = LV_LIFNR
        IM_KUNNR         = space
        IM_VENDOR_VIEW   = space
        IM_CUSTOMER_VIEW = space.
*      IMPORTING
*        EM_ERRO          =
      .
*<--- 06/07/2023 - Migração S4 - DG
      ENDIF.

  ENDCASE.


ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM Z_HANDLE_TOOLBAR  USING    P_OBJECT  TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                P_INTERACTIVE TYPE CHAR1 .

** Constants for button type
  CONSTANTS:
        C_BUTTON_NORMAL           TYPE I VALUE 0        ,
        C_MENU_AND_DEFAULT_BUTTON TYPE I VALUE 1        ,
        C_MENU                    TYPE I VALUE 2        ,
        C_SEPARATOR               TYPE I VALUE 3        ,
        C_RADIO_BUTTON            TYPE I VALUE 4        ,
        C_CHECKBOX                TYPE I VALUE 5        ,
        C_MENU_ENTRY              TYPE I VALUE 6        .

  DATA SL_TOOLBAR TYPE STB_BUTTON.

* Append Seperator
  MOVE C_SEPARATOR  TO SL_TOOLBAR-BUTN_TYPE.
  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.



ENDFORM.                    " Z_HANDLE_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM Z_HANDLE_COMMAND  USING P_UCOMM TYPE SYUCOMM       .

  CASE P_UCOMM.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY .
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I04
*&---------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS INPUT.

  CLEAR IT_SELECTED_ROWS.
  CALL METHOD WA_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR T_SAIDA_SELECTION.
  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE T_SAIDA
          INTO WA_SAIDA
         INDEX WA_SELECTED_ROWS-INDEX.
    MOVE-CORRESPONDING WA_SAIDA TO WA_SAIDA_SELECTION.
    APPEND WA_SAIDA_SELECTION TO T_SAIDA_SELECTION.
  ENDLOOP.

ENDMODULE.                 " get_selected_rows  INPUT


*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
FORM ZF_BDC USING P_DYNBEGIN TYPE ANY
                  P_NAME     TYPE ANY
                  P_VALUE    TYPE ANY.

  IF P_DYNBEGIN EQ 'X'.
    WA_BDC-PROGRAM  = P_NAME.
    WA_BDC-DYNPRO   = P_VALUE.
    WA_BDC-DYNBEGIN = P_DYNBEGIN.

    APPEND WA_BDC
      TO TI_BDC.
  ELSE.
    WA_BDC-FNAM = P_NAME.
    WA_BDC-FVAL = P_VALUE.

    APPEND WA_BDC
      TO TI_BDC.
  ENDIF.

  CLEAR WA_BDC.
ENDFORM.                    " ZF_BDC


*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB                                            *
*&---------------------------------------------------------------------*
FORM CRIA_HTML_CAB .

  DATA: COLUMN         TYPE REF TO CL_DD_AREA,
        COLUMN_1       TYPE REF TO CL_DD_AREA,
        COLUMN_2       TYPE REF TO CL_DD_AREA,
        TABLE_ELEMENT  TYPE REF TO CL_DD_TABLE_ELEMENT,
        TABLE_ELEMENT2 TYPE REF TO CL_DD_TABLE_ELEMENT,
        P_TEXT         TYPE SDYDO_TEXT_ELEMENT,
        P_TEXT_TABLE   TYPE SDYDO_TEXT_TABLE,
        SDYDO_TEXT_ELEMENT(255),
        VG_MES(2), VG_ANO(4),
        VAR_DATA(10),
        QTD TYPE I.

  CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

  CALL METHOD DG_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 1
      BORDER        = '0'
      WIDTH         = '100%'
    IMPORTING
      TABLE         = TABLE_ELEMENT.

  CALL METHOD TABLE_ELEMENT->ADD_COLUMN
    IMPORTING
      COLUMN = COLUMN.

  CALL METHOD TABLE_ELEMENT->SET_COLUMN_STYLE
    EXPORTING
      COL_NO    = 1
      SAP_ALIGN = 'CENTER'
      SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

  P_TEXT = 'Trasladar y Compensar - Facturas Próprias (Argentina)'.
  CALL METHOD COLUMN->ADD_TEXT
    EXPORTING
      TEXT      = P_TEXT
      SAP_STYLE = 'HEADING'.

  CALL METHOD DG_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '100%'
    IMPORTING
      TABLE         = TABLE_ELEMENT2.

  CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
    IMPORTING
      COLUMN = COLUMN_1.

  CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
    IMPORTING
      COLUMN = COLUMN_2.

  CALL METHOD TABLE_ELEMENT2->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_ALIGN    = 'LEFT'
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM.

  CALL METHOD TABLE_ELEMENT2->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 2
      SAP_ALIGN    = 'LEFT'
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM.

  IF P_KUNNR IS NOT INITIAL.
    SDYDO_TEXT_ELEMENT = 'Cliente:'.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.
*
  IF FC_COMP EQ 'X'.
    SDYDO_TEXT_ELEMENT = 'Compensadas: '.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

  IF FC_ACOMP  EQ 'X'.
    SDYDO_TEXT_ELEMENT = 'A compensar:'.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

*  ENDIF.

  CALL METHOD COLUMN_1->ADD_TEXT
    EXPORTING
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.

  CLEAR: P_TEXT_TABLE, SDYDO_TEXT_ELEMENT.

  IF P_KUNNR-LOW IS NOT INITIAL.
    SDYDO_TEXT_ELEMENT = P_KUNNR-LOW.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

  "Chave do país *********
*  IF P_BUDAT  IS NOT INITIAL.
*    SDYDO_TEXT_ELEMENT = P_BUDAT .
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  ENDIF.
  "Grupos *********
  IF P_BUDAT  IS NOT INITIAL.
    CONCATENATE P_BUDAT+6(2) '.' P_BUDAT+4(2) '.' P_BUDAT(4) INTO  VAR_DATA.
    SDYDO_TEXT_ELEMENT = VAR_DATA.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

  CALL METHOD COLUMN_2->ADD_TEXT
    EXPORTING
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.
*  ENDIF.




  PERFORM CONTAINER_HTML.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM                                            *
*&---------------------------------------------------------------------*
FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                    CHANGING URL.

  REFRESH GRAPHIC_TABLE.
  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.
  WHILE L_GRAPHIC_CONV > 255.
    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    APPEND GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
  ENDWHILE.
  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  APPEND GRAPHIC_TABLE.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = URL.
ENDFORM.                    " F_PEGA_IMAGEM.

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML                                           *
*&---------------------------------------------------------------------*
FORM CONTAINER_HTML .

  DATA : DL_LENGTH  TYPE I,                           " Length
         DL_BACKGROUND_ID TYPE SDYDO_KEY VALUE SPACE. " Background_id

  IF DG_HTML_CNTRL IS INITIAL.
    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_HTML1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      DOCUMENT = DG_DYNDOC_ID
      BOTTOM   = SPACE
    IMPORTING
      LENGTH   = DL_LENGTH.

  CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

  CALL METHOD DG_DYNDOC_ID->SET_DOCUMENT_BACKGROUND
    EXPORTING
      PICTURE_ID = DL_BACKGROUND_ID.

  DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

  CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_HTML1
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT                                                 *
*&---------------------------------------------------------------------*
FORM ADD_TEXT USING P_TEXT  TYPE SDYDO_TEXT_ELEMENT
                    P_STYLE TYPE SDYDO_ATTRIBUTE
                    P_SIZE  TYPE SDYDO_ATTRIBUTE
                    P_COLOR TYPE SDYDO_ATTRIBUTE.

* Adding text
  CALL METHOD DG_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT          = P_TEXT
      SAP_STYLE     = P_STYLE
      SAP_FONTSIZE  = P_SIZE
      SAP_COLOR     = P_COLOR
      SAP_FONTSTYLE = CL_DD_AREA=>SANS_SERIF.


ENDFORM.                    " ADD_TEXT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT                                          *
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT_C-REPORT      = SY-REPID.
  GS_VARIANT_C-HANDLE      = SPACE.
  GS_VARIANT_C-LOG_GROUP   = SPACE.
  GS_VARIANT_C-USERNAME    = SPACE.
  GS_VARIANT_C-VARIANT     = SPACE.
  GS_VARIANT_C-TEXT        = SPACE.
  GS_VARIANT_C-DEPENDVARS  = SPACE.

ENDFORM.                    " FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*&      Form  ZENC_MANUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZENC_MANUAL .

  DATA: VL_ENC_MANUAL TYPE C.

  LOOP AT T_SAIDA_SELECTION INTO WA_SAIDA_SELECTION.

    IF ( WA_SAIDA_SELECTION-ENC_MANUAL IS INITIAL ).
      CONTINUE.
    ENDIF.

    UPDATE ZSDYT0049 SET AUGDT = WA_SAIDA_SELECTION-AUGDT_EM
                         AUGBL = WA_SAIDA_SELECTION-AUGBL_EM
       WHERE OBJ_KEY = WA_SAIDA_SELECTION-OBJ_KEY.

    IF SY-SUBRC NE 0.
      MESSAGE TEXT-018 TYPE 'S'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    VL_ENC_MANUAL = 'X'.

  ENDLOOP.

  IF VL_ENC_MANUAL IS NOT INITIAL.
    COMMIT WORK.
    MESSAGE TEXT-019 TYPE 'S'.
    PERFORM ZRENOVAR.
  ENDIF.

ENDFORM.
