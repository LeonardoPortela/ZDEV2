*&---------------------------------------------------------------------*
*&  Include           ZGH_VIP001
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <FS_VALUE> TYPE ANY.
FIELD-SYMBOLS: <FS_ARQTXT> TYPE TABLE.

TYPES: TY_T_XMLTB  TYPE TABLE OF SMUM_XMLTB.

DATA: T_FCAT_H         TYPE LVC_T_FCAT,
      T_FCAT_D         TYPE LVC_T_FCAT,
      T_HEADER         TYPE TABLE OF ZFIT_VIP_INVOICE,
      T_XML_INFO       TYPE TY_T_XMLTB,
      W_XML_INFO       LIKE LINE OF T_XML_INFO,
      VL_HIER          LIKE W_XML_INFO-HIER,
      V_TESTE          TYPE STRING,
      V_VALUE          TYPE STRING,
      V_DETAIL_INDEX   TYPE SY-TABIX,
      V_NEW_INDEX      TYPE SY-TABIX,
      V_CHECK_NEW_LINE TYPE CHAR1 VALUE 'X',
      T_ARQ_IN         TYPE TABLE OF STRING.


FORM FM_PROCESS_XML USING I_XML TYPE STRING
                    CHANGING T_CONTEUDO_XML TYPE ZFIT_T_VIP_INVOICE.

*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      FILENAME = 'C:\TEMP\XML.XML'
*    TABLES
*      DATA_TAB = T_ARQ_IN
*    EXCEPTIONS
*      OTHERS   = 01.
*
*  CONCATENATE LINES OF T_ARQ_IN INTO V_TESTE SEPARATED BY CL_ABAP_CHAR_UTILITIES=>CR_LF.


  PERFORM F_DO_XML_PARSE_1 USING I_XML.

  PERFORM ZF_MONTAR_FIELDCAT     CHANGING T_HEADER T_FCAT_H.
  APPEND INITIAL LINE TO T_HEADER ASSIGNING FIELD-SYMBOL(<FS_HEADER>).

  LOOP AT T_FCAT_H ASSIGNING FIELD-SYMBOL(<FS_FCAT>).

    CHECK <FS_FCAT>-FIELDNAME <> 'INVOICEDETAILS'.

    ASSIGN COMPONENT <FS_FCAT>-FIELDNAME OF STRUCTURE <FS_HEADER> TO <FS_VALUE>.
    CHECK <FS_VALUE> IS ASSIGNED.

    CLEAR V_VALUE.
    PERFORM ZF_GET_TAG USING    'INVOICE' <FS_FCAT>-FIELDNAME  CHANGING V_VALUE V_DETAIL_INDEX.
    <FS_VALUE>      = V_VALUE.

  ENDLOOP.

  PERFORM ZF_MONTAR_FIELDCAT     CHANGING <FS_HEADER>-INVOICEDETAILS T_FCAT_D.

  WHILE V_CHECK_NEW_LINE = 'X'.

    APPEND INITIAL LINE TO <FS_HEADER>-INVOICEDETAILS ASSIGNING FIELD-SYMBOL(<FS_DETAILS>).

    LOOP AT T_FCAT_D ASSIGNING FIELD-SYMBOL(<FS_FCAT_D>).

      ASSIGN COMPONENT <FS_FCAT_D>-FIELDNAME OF STRUCTURE <FS_DETAILS> TO <FS_VALUE>.
      CHECK <FS_VALUE> IS ASSIGNED.

      CLEAR V_VALUE.
      PERFORM ZF_GET_TAG USING    'INVOICEDETAILS' <FS_FCAT_D>-FIELDNAME  CHANGING V_VALUE V_DETAIL_INDEX.
      <FS_VALUE>      = V_VALUE.

      IF ( <FS_FCAT_D>-FIELDNAME = 'VENDORREFERENCECODE' ).
        DELETE T_XML_INFO[] INDEX V_DETAIL_INDEX.

        READ TABLE T_XML_INFO[] WITH KEY CNAME = 'INVOICEDETAILS' TRANSPORTING NO FIELDS.

        IF ( SY-SUBRC = 0 ).
          V_NEW_INDEX = SY-TABIX.
          V_NEW_INDEX = ( V_NEW_INDEX - 1 ).
          DELETE T_XML_INFO FROM 1 TO V_NEW_INDEX.
        ELSE.
          CLEAR: V_CHECK_NEW_LINE.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDWHILE.

  T_CONTEUDO_XML[] = T_HEADER[].

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZF_TRANSFORMA_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_TRANSFORMA_XML CHANGING T_XML_INFO TYPE TY_T_XMLTB.

  DATA: T_RETURN TYPE STANDARD TABLE OF BAPIRET2,
        W_RETURN LIKE LINE OF T_RETURN,
        V_XTRING TYPE XSTRING.


  CLEAR T_XML_INFO.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      TEXT   = V_TESTE
    IMPORTING
      BUFFER = V_XTRING
    EXCEPTIONS
      FAILED = 1
      OTHERS = 2.
  IF SY-SUBRC = 0.

    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        XML_INPUT = V_XTRING
      TABLES
        XML_TABLE = T_XML_INFO
        RETURN    = T_RETURN.
    READ TABLE T_RETURN TRANSPORTING NO FIELDS WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      LOOP AT T_RETURN INTO W_RETURN.
        WRITE: / W_RETURN-MESSAGE.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.                    " ZF_TRANSFORMA_XML

*&---------------------------------------------------------------------*
*&      Form  ZF_GET_TAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_GET_TAG  USING    P_PAI TYPE C
                          P_TAG TYPE C
                 CHANGING P_VALOR TYPE ANY
                          P_DETAIL_INDEX TYPE SY-TABIX.

  DATA: W_XML_INFO LIKE LINE OF T_XML_INFO,
        VL_HIER    LIKE W_XML_INFO-HIER.

  READ TABLE T_XML_INFO INTO W_XML_INFO WITH KEY CNAME = P_PAI.
  IF SY-SUBRC = 0.
    P_DETAIL_INDEX = SY-TABIX.
    VL_HIER = W_XML_INFO-HIER + 1.

    READ TABLE T_XML_INFO INTO W_XML_INFO WITH KEY CNAME = P_TAG
                                                   HIER  = VL_HIER.
    IF SY-SUBRC = 0.
      P_VALOR = W_XML_INFO-CVALUE.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_GET_T
*&---------------------------------------------------------------------*
*&      Form  F_DO_XML_PARSE_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_XML_STR_CONTENT  text
*----------------------------------------------------------------------*
FORM F_DO_XML_PARSE_1  USING P_CONTENT TYPE STRING.

  DATA: "st_ct_xml_data TYPE  gty_s_xml_line,
    VL_TEXT     TYPE STRING,
    XML_STRING  TYPE XSTRING,
    T_XML_TABLE TYPE STANDARD TABLE OF SMUM_XMLTB,
    T_RETURN    TYPE STANDARD TABLE OF BAPIRET2,
    W_RETURN    TYPE BAPIRET2.

  DATA G_IXML TYPE REF TO IF_IXML.

*  LOOP AT p_ct_xml_data INTO st_ct_xml_data.
*
*    vl_text = st_ct_xml_data.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      TEXT   = P_CONTENT
    IMPORTING
      BUFFER = XML_STRING
    EXCEPTIONS
      FAILED = 1
      OTHERS = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      XML_INPUT = XML_STRING
    TABLES
      XML_TABLE = T_XML_TABLE
      RETURN    = T_RETURN.

  READ TABLE T_RETURN TRANSPORTING NO FIELDS WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    LOOP AT T_RETURN INTO W_RETURN.
      WRITE: / W_RETURN-MESSAGE.
    ENDLOOP.
  ELSE.
    T_XML_INFO[] = T_XML_TABLE[].
  ENDIF.

  LOOP AT T_XML_INFO ASSIGNING FIELD-SYMBOL(<FS_INFO>).
    TRANSLATE <FS_INFO>-CNAME TO UPPER CASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM ZF_MONTAR_FIELDCAT  CHANGING PT_TABELA   TYPE ANY TABLE
                                  PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    L_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
    L_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
    L_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
    L_DATA         TYPE REF TO DATA.
  FIELD-SYMBOLS:
    <F_TABLE>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA L_DATA LIKE PT_TABELA.
  ASSIGN L_DATA->* TO <F_TABLE>.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      CL_SALV_TABLE=>FACTORY(
        EXPORTING
          LIST_DISPLAY = ABAP_FALSE
        IMPORTING
          R_SALV_TABLE = L_SALV_TABLE
        CHANGING
          T_TABLE      = <F_TABLE> ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  L_COLUMNS      = L_SALV_TABLE->GET_COLUMNS( ).
  L_AGGREGATIONS = L_SALV_TABLE->GET_AGGREGATIONS( ).

* Monta o fieldcat
  PT_FIELDCAT = CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG( R_COLUMNS      = L_COLUMNS
                                                                   R_AGGREGATIONS = L_AGGREGATIONS ).

ENDFORM.
