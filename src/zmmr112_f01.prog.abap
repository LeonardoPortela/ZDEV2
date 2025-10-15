*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTA_MSC2N                                          *
*&---------------------------------------------------------------------*
*                         Executa Bapi MSC2N                           *
*----------------------------------------------------------------------*
FORM Z_EXECUTA_MSC2N USING  W_MATNR TYPE MARA-MATNR.

  DATA: VL_NUM   TYPE BAPI1003_KEY-CLASSNUM,
        VL_TYPE  TYPE BAPI1003_KEY-CLASSTYPE,
        VL_TABLE TYPE BAPI1003_KEY-OBJECTTABLE,
        VL_KEY   TYPE BAPI1003_KEY-OBJECT,
        SL_KEYS  TYPE BAPI1003_OBJECT_KEYS,
        TL_TABLE TYPE TABLE OF BAPI1003_OBJECT_KEYS,
        TL_RET   TYPE TABLE OF BAPIRET2,
        TL_NUM   TYPE TABLE OF BAPI1003_ALLOC_VALUES_NUM,
        TL_CHAR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
        TL_CURR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CURR.


  SELECT CLINT KLART CLASS VONDT BISDT
  FROM KLAH
    INTO TABLE T_KLAH
  WHERE KLART = '023'
  AND   CLASS = 'MATEPI'.

  CHECK NOT T_KLAH[] IS INITIAL.

  SELECT CLINT POSNR ADZHL IMERK KLART
   FROM KSML
     INTO TABLE T_KSML
     FOR ALL ENTRIES IN T_KLAH
   WHERE CLINT EQ T_KLAH-CLINT
   AND KLART EQ T_KLAH-KLART.

  CHECK NOT T_KSML[] IS INITIAL.

  SELECT A~ATINN A~SPRAS A~ADZHL A~ATBEZ
       B~ATNAM B~ATFOR
      FROM CABNT AS A
        INNER JOIN CABN AS B ON A~ATINN EQ B~ATINN
                            AND A~ADZHL EQ B~ADZHL
        INTO TABLE T_CABNT
        FOR ALL ENTRIES IN T_KSML
      WHERE A~ATINN EQ T_KSML-IMERK
      AND A~SPRAS EQ 'PT'.

  VL_TABLE = 'MARA'.
  VL_NUM  = 'MATEPI'.
  VL_TYPE = '023'.
*---> 04/07/2023 - Migração S4 - AF
* VL_KEY  = W_MATNR.
  VL_KEY  = CONV #( W_MATNR ).
*---> 04/07/2023 - Migração S4 - AF
* Preenche Características
  PERFORM Z_PREENCHE_CARAC TABLES TL_CHAR
                                  TL_NUM.


  CALL FUNCTION 'BAPI_OBJCL_CREATE'"#EC CI_USAGE_OK[2438131]
    EXPORTING
      OBJECTKEYNEW    = VL_KEY
      OBJECTTABLENEW  = VL_TABLE
      CLASSNUMNEW     = VL_NUM
      CLASSTYPENEW    = VL_TYPE
    TABLES
      ALLOCVALUESNUM  = TL_NUM
      ALLOCVALUESCHAR = TL_CHAR
      ALLOCVALUESCURR = TL_CURR
      RETURN          = TL_RET.

  DELETE TL_RET WHERE TYPE EQ 'S'.
  DELETE TL_RET WHERE TYPE EQ 'W'.
** Retorna MSG
*  PERFORM Z_RETORNA_MSG TABLES TL_RET
*                         USING P_MCHB-MATNR
*                               P_MCHB-CHARG
*                               P_MCHB-WERKS.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.

ENDFORM.                    " Z_EXECUTA_MSC2N

*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG                                            *
*&---------------------------------------------------------------------*
*                               Retorna MSG                            *
*----------------------------------------------------------------------*
FORM Z_RETORNA_MSG TABLES P_RET   STRUCTURE BAPIRET2
                    USING P_MATNR TYPE MCHB-MATNR
                          P_CHARG TYPE MCHB-CHARG
                          P_WERKS TYPE MCHB-WERKS.

  DATA: SL_RET TYPE BAPIRET2.


  LOOP AT P_RET INTO SL_RET.
    SL_MSG-MATERIAL = P_MATNR.
    SL_MSG-LOTE     = P_CHARG.
    SL_MSG-CENTRO   = P_WERKS.
    SL_MSG-TIPO     = SL_RET-TYPE.
    SL_MSG-NUMERO   = SL_RET-NUMBER.
    SL_MSG-MENSAGEM = SL_RET-MESSAGE.

    APPEND SL_MSG TO T_MSG.

    CLEAR: SL_RET, SL_MSG.
  ENDLOOP.

ENDFORM.                    " Z_RETORNA_MSG


*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CARAC                                         *
*&---------------------------------------------------------------------*
*                      Preenche Características                        *
*----------------------------------------------------------------------*
FORM Z_PREENCHE_CARAC TABLES P_CHAR  STRUCTURE BAPI1003_ALLOC_VALUES_CHAR
                             P_TNUM  STRUCTURE BAPI1003_ALLOC_VALUES_NUM.

  DATA:
    VL_CAMPO  TYPE CHAR30,
    SL_CHAR   TYPE BAPI1003_ALLOC_VALUES_CHAR,
    SL_NUM    TYPE BAPI1003_ALLOC_VALUES_NUM,
    SL_AUX    TYPE BAPI1003_ALLOC_VALUES_CHAR,
    SL_CABNT  TYPE TYPE_CABNT,
    TL_AUX    TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
    TL_NUM    TYPE TABLE OF BAPI1003_ALLOC_VALUES_NUM,
    TL_CHAR   TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
    TL_CURR   TYPE TABLE OF BAPI1003_ALLOC_VALUES_CURR,
    TL_RET    TYPE TABLE OF BAPIRET2,
    N_UHML(8) TYPE P DECIMALS 2.



  REFRESH: P_CHAR, P_TNUM.


  SORT TL_AUX BY CHARACT_DESCR ASCENDING.

  LOOP AT T_CABNT INTO SL_CABNT.
    CLEAR: P_CHAR, P_TNUM, SL_AUX.
    P_CHAR-CHARACT     = SL_CABNT-ATNAM.
    CASE SL_CABNT-ATNAM.
      WHEN 'ZEPI_CA'.
        P_CHAR-VALUE_CHAR  = WG_CADMAT-EPI_CA.
      WHEN 'ZEPI_PERI'.
        P_CHAR-VALUE_CHAR  = WG_CADMAT-EPI_PERI.
      WHEN 'ZEPI_VALCA'.
        CONCATENATE WG_CADMAT-EPI_VAL+6(2) '.' WG_CADMAT-EPI_VAL+4(2) '.' WG_CADMAT-EPI_VAL+0(4) INTO P_CHAR-VALUE_CHAR.
    ENDCASE.
    APPEND P_CHAR.
    CLEAR: SL_CABNT, SL_AUX, SL_NUM, SL_CHAR.
  ENDLOOP.

ENDFORM.                    " Z_PREENCHE_CARAC

*----------------------------------------------------------------------*
***INCLUDE ZMMR112_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM ENVIA_EMAIL USING P_TIPO.

  DATA: WL_ZMMT0069 TYPE ZMMT0069,
        IT_ZMMT0078 TYPE TABLE OF ZMMT0078,
        WA_ZMMT0078 TYPE ZMMT0078.
  SELECT SINGLE * FROM ZMMT0069 INTO WL_ZMMT0069 WHERE NUREQ = WG_CADMAT-NUREQ.

  DATA: GT_DESTINATARIO TYPE STANDARD TABLE OF SOMLREC90 WITH HEADER LINE,
        GT_ASSUNTO      TYPE SODOCCHGI1,
        GT_TEXTO        TYPE STANDARD TABLE OF SOLI WITH HEADER LINE.

  REFRESH: GT_DESTINATARIO,
           GT_TEXTO.
  CLEAR    GT_ASSUNTO.
***********************
* Objetos
***********************
  DATA: OBJ_ZCL_UTIL TYPE REF TO ZCL_UTIL.

***********************
* Variavel
***********************
  DATA: VAR_DATA    TYPE SY-DATUM,
        VAR_DATA_I  TYPE SY-DATUM,
        VLR_STRING  TYPE STRING,
        VAR_DATA_BR TYPE C LENGTH 10.

  DATA: BSMTP_ADDR TYPE ADR6-SMTP_ADDR.

  IF P_TIPO = 'H' AND WG_CADMAT2-HOM_EPI = 'X'. "Se for homologação de material EPI e homologado pelo SSO filial, manda email para SSO matriz
    SELECT *
    FROM ZMMT0078
    INTO TABLE IT_ZMMT0078
    WHERE MTART = 'ZSSO'.
    LOOP AT IT_ZMMT0078 INTO WA_ZMMT0078.
      SELECT SINGLE ADR6~SMTP_ADDR INTO BSMTP_ADDR
        FROM USR21
          INNER JOIN ADR6
             ON  USR21~ADDRNUMBER = ADR6~ADDRNUMBER
            AND USR21~PERSNUMBER = ADR6~PERSNUMBER
                WHERE USR21~BNAME =  WA_ZMMT0078-USNAM.
      IF SY-SUBRC = 0.
        GT_DESTINATARIO-REC_TYPE = 'U'.
        GT_DESTINATARIO-RECEIVER = BSMTP_ADDR.
        APPEND GT_DESTINATARIO.
      ENDIF.
    ENDLOOP.
  ELSE.
    SELECT SINGLE ADR6~SMTP_ADDR INTO BSMTP_ADDR
      FROM USR21
        INNER JOIN ADR6
           ON  USR21~ADDRNUMBER = ADR6~ADDRNUMBER
          AND USR21~PERSNUMBER = ADR6~PERSNUMBER
              WHERE USR21~BNAME =  WL_ZMMT0069-SOLIC.
    GT_DESTINATARIO-REC_TYPE = 'U'.
    GT_DESTINATARIO-RECEIVER = BSMTP_ADDR.
    APPEND GT_DESTINATARIO.

  ENDIF.

  GT_ASSUNTO-OBJ_NAME = 'MATERIAL'.
  CONCATENATE 'Solicitação' WL_ZMMT0069-NUREQ INTO GT_ASSUNTO-OBJ_DESCR.
  GT_ASSUNTO-OBJ_LANGU = SY-LANGU.

  IF P_TIPO = 'H'.
    GT_TEXTO = 'Material homologado'.
  ELSEIF P_TIPO = 'R'.
    GT_TEXTO = 'Material não homologado.'.
    GT_TEXTO = 'Verificar o motivo da recusa'.
  ELSEIF P_TIPO = 'P'.
    GT_TEXTO = 'Material para solicitação gerado o codigo'.
    GT_TEXTO =  WL_ZMMT0069-MATNRG.
    GT_TEXTO =  'Por favor, acompanhar no WORKPLACE'.
  ENDIF.
  APPEND GT_TEXTO.

  "Enviar E-mail
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = GT_ASSUNTO
      DOCUMENT_TYPE              = 'HTM'
    TABLES
      OBJECT_CONTENT             = GT_TEXTO
      RECEIVERS                  = GT_DESTINATARIO
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.

ENDFORM.                    " ENVIA_EMAIL
"
FORM F_BUSCA_DADOS .
  DATA: RA_WERKS TYPE RANGE OF ZMMT0069-WERKS WITH HEADER LINE.
  DATA: RA_DATA TYPE RANGE OF ZMMT0069-DT_ENTRADA WITH HEADER LINE.
  DATA: VHOMOLOG TYPE ZMMT0069-HOMOLOG.

  IF WG_CADMAT2-WERKS IS NOT INITIAL.
    RA_WERKS-SIGN = 'I'.
    RA_WERKS-OPTION = 'EQ'.
    RA_WERKS-LOW =  WG_CADMAT2-WERKS.
    APPEND RA_WERKS.
  ENDIF.

  CLEAR WA_ZMMT0078.
  REFRESH IT_ZMMT0078.

  SELECT *
    FROM ZMMT0078
    INTO  TABLE IT_ZMMT0078
    WHERE USNAM EQ SY-UNAME
    AND   MTART EQ 'ZEPI'
    AND   WERKS EQ '9999'. "aprova Todos

  IF IT_ZMMT0078[] IS INITIAL. " se não houver para todos
    SELECT *
      FROM ZMMT0078
      INTO  TABLE IT_ZMMT0078
      WHERE USNAM EQ SY-UNAME
      AND   MTART EQ 'ZEPI'
      AND   WERKS NE ''.

    IF IT_ZMMT0078[] IS NOT INITIAL.
      REFRESH  RA_WERKS.
      LOOP AT IT_ZMMT0078 INTO WA_ZMMT0078.
        RA_WERKS-SIGN = 'I'.
        RA_WERKS-OPTION = 'EQ'.
        RA_WERKS-LOW =  WA_ZMMT0078-WERKS.
        APPEND RA_WERKS.
      ENDLOOP.

    ENDIF.
  ENDIF.


  IF WG_CADMAT2-DT_ENTRADA IS NOT INITIAL AND WG_CADMAT2-FINAL IS INITIAL.
    RA_DATA-SIGN    = 'I'.
    RA_DATA-OPTION  = 'EQ'.
    RA_DATA-LOW     =  WG_CADMAT2-DT_ENTRADA.
    APPEND RA_DATA.
  ELSEIF  WG_CADMAT2-DT_ENTRADA IS NOT INITIAL AND WG_CADMAT2-FINAL IS NOT INITIAL.
    RA_DATA-SIGN   = 'I'.
    RA_DATA-OPTION = 'BT'.
    RA_DATA-LOW    =  WG_CADMAT2-DT_ENTRADA.
    RA_DATA-HIGH   =  WG_CADMAT2-DT_FINAL.
    APPEND RA_DATA.
  ENDIF.

  IF WG_CADMAT2-HOM_TI = 'X'.
    VHOMOLOG = 'T'.
  ELSEIF WG_CADMAT2-HOM_SSO = 'X'.
    VHOMOLOG = 'O'.
  ELSEIF WG_CADMAT2-HOM_EPI = 'X'.
    VHOMOLOG = 'E'.
  ELSE.
    VHOMOLOG = 'S'.
  ENDIF.


  IF WG_CADMAT2-NOVOS = 'X'.
    SELECT *
      FROM ZMMT0069
      INTO TABLE IT_ZMMT0069
      WHERE WERKS IN RA_WERKS
      AND   DT_ENTRADA  IN RA_DATA
      AND   RECUSA_FLAG EQ WG_CADMAT2-RECUSA_S
      AND   ELIMINADO   EQ ''
      AND   EXPANSAO    EQ ''
      AND   HOMOLOG     EQ VHOMOLOG
      AND   ( MATNRG    EQ '' OR GER_WF EQ '' ).
  ELSE.
    SELECT *
     FROM ZMMT0069
     INTO TABLE IT_ZMMT0069
     WHERE WERKS IN RA_WERKS
     AND   DT_ENTRADA  IN RA_DATA
     AND   RECUSA_FLAG EQ WG_CADMAT2-RECUSA_S
     AND   ELIMINADO   EQ ''
     AND   EXPANSAO    EQ ''
     AND   HOMOLOG     EQ VHOMOLOG
     AND   ( MATNRG    NE '' AND GER_WF EQ 'X' ).

  ENDIF.

  REFRESH IT_SAIDA.
  LOOP AT IT_ZMMT0069.
    MOVE-CORRESPONDING IT_ZMMT0069 TO WA_SAIDA.
    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.


  SELECT *
    FROM  SETLEAF
    INTO TABLE T_SET
    WHERE SETCLASS      = '0000'
    AND   SETNAME        = 'MAGGI_ZWFMAT009'.

  SELECT *
    FROM SETLINET
    INTO TABLE T_LAY
    FOR ALL ENTRIES IN T_SET
    WHERE SETCLASS   = T_SET-SETCLASS
    AND SUBCLASS     = T_SET-SUBCLASS
    AND SETNAME      = T_SET-SETNAME
    AND LANGU        = 'P'
    AND LINEID       = T_SET-LINEID.

ENDFORM.

FORM F_BUSCA_0200 .
  DATA: WL_ZMMT0069  TYPE ZMMT0069,
        WL_MARA      TYPE MARA,
        WL_MAKT      TYPE MAKT,
        WL_CONT      TYPE SY-TABIX,
        WL_CONT_AUX  TYPE SY-TABIX,
        WL_CONT_AUX2 TYPE SY-TABIX,
        V_MATNR      TYPE MARA-MATNR.

  CHECK WA_SAIDA-NUREQ IS NOT INITIAL.
  SELECT SINGLE * FROM ZMMT0069
    INTO WL_ZMMT0069
  WHERE NUREQ EQ WA_SAIDA-NUREQ.


  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
   ID 'WERKS' FIELD  WL_ZMMT0069-WERKS
   ID 'ACTVT' FIELD '03'.    "Alteração

  CASE SY-SUBRC.
    WHEN 0.

    WHEN 4.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Sem autorização para este centro'.
      EXIT.
    WHEN 12.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Sem autorização neste objeto'.
      EXIT.
    WHEN OTHERS.
  ENDCASE.

  MOVE-CORRESPONDING WL_ZMMT0069 TO WG_CADMAT.

  IF WG_CADMAT-MATKL IS NOT INITIAL.
    SELECT SINGLE *
      FROM T023T
      INTO WA_T023T
      WHERE SPRAS EQ SY-LANGU
      AND   MATKL EQ WG_CADMAT-MATKL.

    WG_CADMAT-WGBEZ = WA_T023T-WGBEZ.

  ENDIF.
  IF WG_CADMAT-MTART IS NOT INITIAL.
    SELECT SINGLE *
      FROM T134T
      INTO WA_T134T
      WHERE SPRAS EQ SY-LANGU
      AND   MTART EQ WG_CADMAT-MTART.

    WG_CADMAT-MTBEZ = WA_T134T-MTBEZ.

  ENDIF.
  IF WG_CADMAT-MBRSH IS NOT INITIAL.
    SELECT SINGLE *
      FROM T137T
      INTO WA_T137T
      WHERE SPRAS EQ SY-LANGU
      AND   MBRSH EQ WG_CADMAT-MBRSH.

    WG_CADMAT-MBBEZ = WA_T137T-MBBEZ.

  ENDIF.

  IF WG_CADMAT-CODAGREPI IS NOT INITIAL.
    SELECT SINGLE DENAGREPI
         FROM ZHRST_MED_AG_EPI
         INTO WG_CADMAT-DENAGREPI
        WHERE CODAGREPI = WG_CADMAT-CODAGREPI.
  ENDIF.


  REFRESH  T_LINE. "textos básicos
  "Observação
  REFRESH: TG_EDITOR.
  CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
  WL_CONT = STRLEN( WL_ZMMT0069-OBSERV ).
  WL_CONT_AUX = WL_CONT / 72.

  DO.
    MOVE: WL_ZMMT0069-OBSERV+WL_CONT_AUX2 TO WG_EDITOR-LINE.
    ADD 72 TO WL_CONT_AUX2.
    APPEND WG_EDITOR TO TG_EDITOR.

    " Textos básicos
    T_LINE-TDFORMAT = 'ST'.
    T_LINE-TDLINE = WG_EDITOR.
    APPEND T_LINE.
    "
    IF WL_CONT_AUX2 GT WL_CONT.
      EXIT.
    ENDIF.
  ENDDO.

  CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = TG_EDITOR.
  CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
    EXPORTING
      READONLY_MODE = 0.
  "Recusa
  REFRESH: TG_EDITOR.
  CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
  WL_CONT = STRLEN( WL_ZMMT0069-RECUSA ).
  WL_CONT_AUX = WL_CONT / 72.

  DO.
    MOVE: WL_ZMMT0069-RECUSA+WL_CONT_AUX2 TO WG_EDITOR-LINE.
    ADD 72 TO WL_CONT_AUX2.
    APPEND WG_EDITOR TO TG_EDITOR.

    IF WL_CONT_AUX2 GT WL_CONT.
      EXIT.
    ENDIF.
  ENDDO.

  CALL METHOD OBG_DESCBOXR->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = TG_EDITOR.
  CALL METHOD OBG_DESCBOXR->SET_READONLY_MODE
    EXPORTING
      READONLY_MODE = 0.

  IF WG_CADMAT-MATNR IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADMAT-MATNR
      IMPORTING
        OUTPUT = WG_CADMAT-MATNR.
    SELECT SINGLE *
      FROM MARA
      INTO WL_MARA
      WHERE MATNR = WG_CADMAT-MATNR.
    IF SY-SUBRC NE 0.
      MESSAGE 'Material não existe!' TYPE 'I'.
      CLEAR: WG_CADMAT-MATNR,
             WG_CADMAT-MAKTX,
             WG_CADMAT-MATKL,
             WG_CADMAT-MEINS.
      EXIT.
    ENDIF.
    SELECT SINGLE *
      FROM MAKT
      INTO WL_MAKT
      WHERE MATNR = WG_CADMAT-MATNR
      AND   SPRAS = SY-LANGU.

    WG_CADMAT-MAKTX = WL_MAKT-MAKTX.
    WG_CADMAT-MATKL = WL_MARA-MATKL.
    WG_CADMAT-MEINS = WL_MARA-MEINS.
    WG_CADMAT-MBRSH = WL_MARA-MBRSH.
    WG_CADMAT-MTART = WL_MARA-MTART.

  ENDIF.


ENDFORM.

FORM F_TRATA_CAMPOS  USING P_FIELD P_GROUP1 P_VALUE P_INVISIBLE.
  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.
ENDFORM.                    " F_TRATA_CAMPOS
*&----------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LIMPA_CAMPOS .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MM01  USING PA_SAIDA TYPE TY_SAIDA CHANGING  W_MATNR TYPE MARA-MATNR.
  DATA: SL_MCHB TYPE TYPE_MCHB.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
        'SAPLMGMM'  '0060'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=ENTR',
        ''          ''      ''   'RMMG1-MBRSH'        WG_CADMAT-MBRSH,
        ''          ''      ''   'RMMG1-MTART'        WG_CADMAT-MTART,

        'SAPLMGMM'  '0070'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=ENTR',
        ''          ''      ''   'MSICHTAUSW-KZSEL(01)'	      'X',

        'SAPLMGMM'  '4004'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=BU',
        ''          ''      ''   'MAKT-MAKTX'	      WG_CADMAT-MAKTX,
        ''          ''      ''   'MARA-MEINS'	      WG_CADMAT-MEINS,
        ''          ''      ''   'MARA-MATKL'	      WG_CADMAT-MATKL.
  READ TABLE T_SET WITH KEY VALFROM = WG_CADMAT-MATKL.

  IF SY-SUBRC = 0.
    READ TABLE T_LAY WITH KEY  SETCLASS   = T_SET-SETCLASS
                               SUBCLASS   = T_SET-SUBCLASS
                               SETNAME    = T_SET-SETNAME
                               LINEID     = T_SET-LINEID.
    IF SY-SUBRC = 0.
      PERFORM F_BDC_DATA USING:
          ''          ''      ''   'MARA-EXTWG'	      T_LAY-DESCRIPT+0(3).
    ENDIF.
  ENDIF.

  CLEAR W_MATNR.
  PERFORM ZF_CALL_TRANSACTION USING 'MM01' CHANGING W_MATNR.
  COMMIT WORK.
  WAIT UP TO 3 SECONDS.
  "TEXTO
  REFRESH  T_LINE. "textos básicos
  REFRESH: TG_EDITOR.
  IF OBG_DESCBOX IS NOT INITIAL.
    CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.

    LOOP AT TG_EDITOR INTO WG_EDITOR.
      IF WG_EDITOR-LINE IS NOT INITIAL.
        T_LINE-TDFORMAT = 'ST'.
        T_LINE-TDLINE = WG_EDITOR-LINE.
        APPEND T_LINE.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W_MATNR
    IMPORTING
      OUTPUT = W_MATNR.

  L_ID = 'GRUN'.
  L_NAME = W_MATNR.
  CALL FUNCTION 'CREATE_TEXT'
    EXPORTING
      FID       = L_ID
      FLANGUAGE = SY-LANGU
      FNAME     = L_NAME
      FOBJECT   = W_OBJECT
*     SAVE_DIRECT       = 'X'
*     FFORMAT   = '*'
    TABLES
      FLINES    = T_LINE
    EXCEPTIONS
      NO_INIT   = 1
      NO_SAVE   = 2
      OTHERS    = 3.

  IF  WG_CADMAT-MTART = 'ZEPI'.
    PERFORM: Z_EXECUTA_MSC2N USING   W_MATNR.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .

  REFRESH IT_FIELDCAT.

  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NUREQ'.
  WA_AFIELD-SCRTEXT_S = 'Solicitação'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WERKS'.
  WA_AFIELD-SCRTEXT_S = 'Centro'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VKORG'.
  WA_AFIELD-SCRTEXT_S = 'Org.Vd'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VTWEG'.
  WA_AFIELD-SCRTEXT_S = 'Canal'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATNR'.
  WA_AFIELD-SCRTEXT_S = 'Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MAKTX'.
  WA_AFIELD-SCRTEXT_S = 'Texto breve'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MEINS'.
  WA_AFIELD-SCRTEXT_S = 'Unid.Medida'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATKL'.
  WA_AFIELD-SCRTEXT_S = 'Grupo Merc.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SOLIC'.
  WA_AFIELD-SCRTEXT_S = 'Solicitante'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_ENTRADA'.
  WA_AFIELD-SCRTEXT_S = 'Data Sol.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'HR_ENTRADA'.
  WA_AFIELD-SCRTEXT_S = 'Hora Sol.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATNRG'.
  WA_AFIELD-SCRTEXT_S = 'Material Criado'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CRIADO'.
  WA_AFIELD-SCRTEXT_S = 'Criado por'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_CRIACAO'.
  WA_AFIELD-SCRTEXT_S = 'Data Cria'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'HR_CRIACAO'.
  WA_AFIELD-SCRTEXT_S = 'Hora Cria'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RECUSA_FLAG'.
  WA_AFIELD-SCRTEXT_S = 'Recusado?'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.

FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM   = P_PROGRAM.
  WA_BDCDATA-DYNPRO    = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN  = P_START.
  WA_BDCDATA-FNAM      = P_FNAM.
  WA_BDCDATA-FVAL      = P_FVAL.
  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    " F_BDC_DATA

FORM ZF_CALL_TRANSACTION USING P_TRANS CHANGING P_MATNR.
  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'M3',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '800'.

  DATA: WL_MODE(1).

  REFRESH IT_MSG.
  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
        MODE WL_MODE
        MESSAGES INTO IT_MSG.


  READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                             MSGNR = C_MSGNR
                             MSGTYP = 'S'.

  IF SY-SUBRC = 0.
    MOVE IT_MSG-MSGV1 TO P_MATNR.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .
  DATA: WA_MARA     TYPE MARA,
        WA_MARC     TYPE MARC,
        WA_T023     TYPE T023,
        WA_T137     TYPE T137,
        WA_T134     TYPE T134,
        WA_ZMMT0069 TYPE ZMMT0069,
        V_NUREQ(12).

  CLEAR:    TG_MSG_RET.
  REFRESH:  TG_MSG_RET.
  IF WG_CADMAT2-HOM_EPI = 'X' AND  OK-CODE NE 'RECUSA'.
    IF WG_CADMAT-EPI_CA IS INITIAL.
      MOVE: 'WG_CADMAT-EPI_CA'      TO TG_MSG_RET-FIELD,
      TEXT-I06           TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
    IF WG_CADMAT-EPI_PERI IS INITIAL.
      MOVE: 'WG_CADMAT-EPI_PERI'      TO TG_MSG_RET-FIELD,
      TEXT-I07           TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_CADMAT-EPI_VAL IS INITIAL.
      MOVE: 'WG_CADMAT-EPI_VAL'      TO TG_MSG_RET-FIELD,
      TEXT-I08           TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.


  SELECT SINGLE * FROM ZMMT0069 INTO WA_ZMMT0069 WHERE NUREQ = WG_CADMAT-NUREQ.
  IF WA_ZMMT0069-RECUSA_FLAG IS NOT INITIAL.
    MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
    TEXT-I03           TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.

  IF WA_ZMMT0069-ELIMINADO IS NOT INITIAL.
    MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
    TEXT-I01           TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.
  IF OK-CODE = 'RECUSA'.
    IF WA_ZMMT0069-MATNRG IS NOT INITIAL.
      MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
      TEXT-I02           TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.


    REFRESH: TG_EDITOR.
    IF OBG_DESCBOXR IS NOT INITIAL.
      CALL METHOD OBG_DESCBOXR->GET_TEXT_AS_R3TABLE
        IMPORTING
          TABLE = TG_EDITOR.
    ENDIF.

    IF LINES( TG_EDITOR ) GT 13.
      MOVE: 'WG_CADMAT-WERKS'           TO TG_MSG_RET-FIELD,
     'descrição recusa com mais de 13 linhas'  TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF TG_EDITOR[] IS INITIAL.
      MOVE: 'WG_CADMAT-WERKS' TO TG_MSG_RET-FIELD,
      TEXT-I05                TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

  ELSE.
    REFRESH: TG_EDITOR.
    IF OBG_DESCBOX IS NOT INITIAL.
      CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
        IMPORTING
          TABLE = TG_EDITOR.
    ENDIF.

    IF LINES( TG_EDITOR ) GT 13.
      MOVE: 'WG_CADMAT-WERKS'           TO TG_MSG_RET-FIELD,
     'descrição com mais de 13 linhas'  TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_CADMAT-WERKS IS INITIAL.
      MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
      'Informe o Centro'           TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
          ID 'WERKS' FIELD  WG_CADMAT-WERKS
          ID 'ACTVT' FIELD '03'.    "Alteração

      CASE SY-SUBRC.
        WHEN 0.

        WHEN 4.
          MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
          'Sem autorização para este centro'           TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        WHEN 12.
          MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
         'Sem autorização neste objeto'           TO TG_MSG_RET-MSG.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    IF WG_CADMAT-MATKL IS INITIAL.
      MOVE: 'WG_CADMAT-MATKL'      TO TG_MSG_RET-FIELD,
      'Informe Grupo de Material'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
      FROM T023
      INTO WA_T023
      WHERE MATKL = WG_CADMAT-MATKL.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADMAT-MTART'      TO TG_MSG_RET-FIELD,
        'Grupo de Material não existe!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WG_CADMAT-MBRSH IS INITIAL.
      MOVE: 'WG_CADMAT-MBRSH'      TO TG_MSG_RET-FIELD,
      'Informe Setor Industrial'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
        FROM T137
        INTO WA_T137
        WHERE MBRSH = WG_CADMAT-MBRSH.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADMAT-MBRSH'      TO TG_MSG_RET-FIELD,
        'Setor Industrial não existe!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WG_CADMAT-MTART IS INITIAL.
      MOVE: 'WG_CADMAT-MTART'      TO TG_MSG_RET-FIELD,
      'Informe tipo de Material'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
        FROM T134
        INTO WA_T134
        WHERE MTART = WG_CADMAT-MTART.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADMAT-MTART'      TO TG_MSG_RET-FIELD,
        'Tipo de Material não existe!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSEIF WG_CADMAT-MTART = 'ZEPI'.
        IF WG_CADMAT-CODAGREPI IS INITIAL.
          MOVE: 'WG_CADMAT-CODAGREPI'      TO TG_MSG_RET-FIELD,
          'Informe o agrupador de EPI!'      TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          SELECT SINGLE DENAGREPI
             FROM ZHRST_MED_AG_EPI
             INTO WG_CADMAT-DENAGREPI
            WHERE CODAGREPI = WG_CADMAT-CODAGREPI.
          IF SY-SUBRC NE 0.
            MOVE: 'WG_CADMAT-CODAGREPI'      TO TG_MSG_RET-FIELD,
            'Agrupador de EPI não existe!'   TO TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF WG_CADMAT-MAKTX IS INITIAL.
      MOVE: 'WG_CADMAT-MAKTX'      TO TG_MSG_RET-FIELD,
      'Informe o texto breve'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
    IF WG_CADMAT-MEINS IS INITIAL.
      MOVE: 'WG_CADMAT-MEINS'      TO TG_MSG_RET-FIELD,
      'Informe a unidade de medida'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.


    IF WG_CADMAT-VKORG IS INITIAL AND NOT WG_CADMAT-VTWEG IS INITIAL.
      MOVE: 'WG_CADMAT-VKORG'      TO TG_MSG_RET-FIELD,
       'Informar Organização de vendas.'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_CADMAT-VTWEG IS INITIAL AND NOT WG_CADMAT-VKORG IS INITIAL.
      MOVE: 'WG_CADMAT-VKORG'      TO TG_MSG_RET-FIELD,
      'Informar canal de distribuição.'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF TG_MSG_RET[] IS INITIAL AND WG_CADMAT-MATNR IS NOT INITIAL.
      PERFORM F_VALIDA_ENTRADA.
    ENDIF.

  ENDIF.

ENDFORM.

FORM F_VALIDA_ENTRADA .
  "verificar se iniciado
  TYPES: BEGIN OF TY_ZTWF_MAT_LOG,
           MATNR      TYPE ZTWF_MAT_LOG-MATNR,
           WERKS      TYPE ZTWF_MAT_LOG-WERKS,
           DATA       TYPE ZTWF_MAT_LOG-DATA,
           VKORG      TYPE ZTWF_MAT_LOG-VKORG,
           VTWEG      TYPE ZTWF_MAT_LOG-VTWEG,
           NSEQU      TYPE ZTWF_MAT_LOG-NSEQU,
           HORA       TYPE ZTWF_MAT_LOG-HORA,
           USNAM      TYPE ZTWF_MAT_LOG-USNAM,
           MTART      TYPE MARA-MTART,
           CONT       TYPE I,
           SEQ        TYPE I,
           STATUS(10),
         END OF TY_ZTWF_MAT_LOG,

         BEGIN OF TY_ZTWF_MAT_SEQ,
           MTART TYPE ZTWF_MAT_SEQ-MTART,
           WERKS TYPE ZTWF_MAT_SEQ-WERKS,
           NSEQU TYPE ZTWF_MAT_SEQ-NSEQU,
           VKORG TYPE ZTWF_MAT_SEQ-VKORG,
           VTWEG TYPE ZTWF_MAT_SEQ-VTWEG,
           AREA  TYPE ZTWF_MAT_SEQ-AREA,
         END OF TY_ZTWF_MAT_SEQ.

  DATA: T_LOG              TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_LOG_ANT          TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_LOG_AUX          TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_LOG_AUX2         TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_SEQ              TYPE TABLE OF TY_ZTWF_MAT_SEQ,
        T_ZTWF_MAT_AREA_US TYPE ZTWF_MAT_AREA_US   OCCURS 0 WITH HEADER LINE.

  DATA: WA_LOG      TYPE TY_ZTWF_MAT_LOG,
        WA_LOG_AUX  TYPE TY_ZTWF_MAT_LOG,
        WA_LOG_AUX2 TYPE TY_ZTWF_MAT_LOG,
        WA_SEQ      TYPE TY_ZTWF_MAT_SEQ,
        TABIX       TYPE SY-TABIX,
        VMATNR      TYPE MARA-MATNR,
        W_DATA      TYPE SY-DATUM,
        W_CONT      TYPE I.


  REFRESH  T_LOG.
  W_DATA = SY-DATUM - 30.

  SELECT ZTWF_MAT_LOG~MATNR ZTWF_MAT_LOG~WERKS
         ZTWF_MAT_LOG~DATA ZTWF_MAT_LOG~VKORG
         ZTWF_MAT_LOG~VTWEG ZTWF_MAT_LOG~NSEQU ZTWF_MAT_LOG~HORA
         ZTWF_MAT_LOG~USNAM MARA~MTART
   FROM ZTWF_MAT_LOG
    INNER JOIN MARA ON MARA~MATNR = ZTWF_MAT_LOG~MATNR
     INTO TABLE T_LOG
   WHERE  ZTWF_MAT_LOG~MATNR EQ WG_CADMAT-MATNR
     AND  ZTWF_MAT_LOG~WERKS EQ WG_CADMAT-WERKS
*     AND  ZTWF_MAT_LOG~VKORG EQ WG_CADMAT-VKORG
*     AND  ZTWF_MAT_LOG~VTWEG EQ WG_CADMAT-VTWEG
     AND  ZTWF_MAT_LOG~LOEKZ NE 'X'
     AND  ZTWF_MAT_LOG~DATA  GE W_DATA. "Apaga Workflow menos de 30 dias


  CHECK T_LOG[] IS NOT INITIAL.

  " Faz busca nos LOG unicos para identificar se é inicio ou fim de atendimento da sequencia
  T_LOG_AUX[]  = T_LOG[].
  SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA.
  DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU.
  LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
    W_CONT = 0.
    TABIX = SY-TABIX.
    LOOP AT T_LOG INTO WA_LOG WHERE WERKS = WA_LOG_AUX-WERKS
                                     AND   MATNR = WA_LOG_AUX-MATNR
                                     AND   VKORG = WA_LOG_AUX-VKORG
                                     AND   VTWEG = WA_LOG_AUX-VTWEG
                                     AND   NSEQU = WA_LOG_AUX-NSEQU.
      ADD 1 TO W_CONT.
    ENDLOOP.
    WA_LOG_AUX-CONT = W_CONT.
    MODIFY T_LOG_AUX FROM WA_LOG_AUX INDEX TABIX TRANSPORTING CONT.
  ENDLOOP.
  DELETE T_LOG_AUX WHERE CONT NE 1.

  IF T_LOG_AUX[] IS NOT INITIAL.
    W_DATA = SY-DATUM - 10. "até 10 dias considera mesmo atendimento
    SELECT MATNR WERKS DATA VKORG VTWEG NSEQU HORA
    FROM ZTWF_MAT_LOG
      INTO TABLE T_LOG_AUX2
      FOR ALL ENTRIES IN T_LOG_AUX
        WHERE MATNR EQ T_LOG_AUX-MATNR
          AND WERKS EQ T_LOG_AUX-WERKS
          AND VKORG EQ T_LOG_AUX-VKORG
          AND VTWEG EQ T_LOG_AUX-VTWEG
          AND DATA  GT W_DATA
          AND DATA  LT SY-DATUM.

    SORT: T_LOG_AUX2  BY WERKS MATNR VKORG VTWEG ASCENDING DATA DESCENDING  NSEQU DESCENDING.

    " Se ultimo LOG é da mesma sequencia, significa que é o ultimo atendimento o primeiro select
    LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
      READ TABLE T_LOG_AUX2 INTO WA_LOG_AUX2 WITH KEY WERKS = WA_LOG_AUX-WERKS
                                                      MATNR = WA_LOG_AUX-MATNR
                                                      VKORG = WA_LOG_AUX-VKORG
                                                      VTWEG = WA_LOG_AUX-VTWEG.
      IF SY-SUBRC = 0.
        IF WA_LOG_AUX2-NSEQU = WA_LOG_AUX-NSEQU.
          APPEND WA_LOG_AUX2 TO T_LOG.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SELECT MTART WERKS NSEQU VKORG VTWEG AREA
      FROM ZTWF_MAT_SEQ
        INTO TABLE T_SEQ
        FOR ALL ENTRIES IN T_LOG
          WHERE "MTART EQ WMARA-MTART
                WERKS EQ T_LOG-WERKS
            AND NSEQU EQ T_LOG-NSEQU
            AND VKORG EQ T_LOG-VKORG
            AND VTWEG EQ T_LOG-VTWEG.
  ENDIF.

  T_LOG_AUX[]  = T_LOG[].
  T_LOG_AUX2[] = T_LOG[].

  "Organiza dados
  DATA: VSEQ     TYPE I,
        VCONT    TYPE I,
        DATA_INI TYPE SY-DATUM,
        DATA_FIM TYPE SY-DATUM,
        INICIO   TYPE ZTWF_MAT_LOG-HORA,
        FIM      TYPE ZTWF_MAT_LOG-HORA.

  SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
        T_LOG_AUX2 BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
        T_SEQ      BY MTART WERKS NSEQU VKORG VTWEG.

  DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU.

  LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
    VSEQ  = 1.
    VCONT = 0.
    LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                         AND   MATNR = WA_LOG_AUX-MATNR
                                         AND   VKORG = WA_LOG_AUX-VKORG
                                         AND   VTWEG = WA_LOG_AUX-VTWEG
                                         AND   NSEQU = WA_LOG_AUX-NSEQU.
      ADD 1 TO  VCONT.
      IF VCONT = 3.
        VCONT = 1.
        ADD 1 TO VSEQ.
      ENDIF.
      WA_LOG_AUX2-SEQ = VSEQ.
      MODIFY T_LOG_AUX2 FROM WA_LOG_AUX2 INDEX SY-TABIX TRANSPORTING SEQ.
    ENDLOOP.
  ENDLOOP.

  T_LOG_AUX[] = T_LOG_AUX2[].
  SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG  NSEQU SEQ DATA  HORA,
        T_LOG_AUX2 BY WERKS MATNR VKORG VTWEG  NSEQU SEQ DATA  HORA.

  DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU SEQ.

  LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
    TABIX = SY-TABIX.
    LOOP AT  T_SEQ INTO WA_SEQ WHERE  MTART = WA_LOG_AUX-MTART
                                AND   WERKS = WA_LOG_AUX-WERKS
                                AND   NSEQU = WA_LOG_AUX-NSEQU
                                AND   VKORG = WA_LOG_AUX-VKORG
                                AND   VTWEG = WA_LOG_AUX-VTWEG.
      " obtem Data inicio e Data Fim
      CLEAR: DATA_FIM, FIM.
      CLEAR: DATA_INI, INICIO.

      LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                          AND   MATNR = WA_LOG_AUX-MATNR
                                          AND   VKORG = WA_LOG_AUX-VKORG
                                          AND   VTWEG = WA_LOG_AUX-VTWEG
                                          AND   NSEQU = WA_LOG_AUX-NSEQU
                                          AND   SEQ   = WA_LOG_AUX-SEQ.
        IF DATA_INI IS INITIAL. "1
          DATA_INI    = WA_LOG_AUX2-DATA.
          INICIO      = WA_LOG_AUX2-HORA.
        ENDIF.
      ENDLOOP.
      IF WA_LOG_AUX2-DATA = DATA_INI AND
         WA_LOG_AUX2-HORA = INICIO.
        WA_LOG_AUX-STATUS   = 'Pendente'.
      ELSE.
        WA_LOG_AUX-STATUS   = 'Finalizado'.
      ENDIF.
      MODIFY T_LOG_AUX FROM WA_LOG_AUX INDEX TABIX TRANSPORTING STATUS.
    ENDLOOP.

  ENDLOOP.

  DELETE T_LOG_AUX WHERE STATUS   NE 'Finalizado'.

  IF T_LOG_AUX[] IS  INITIAL.
    MOVE: 'WG_CADMAT-MATNR'      TO TG_MSG_RET-FIELD,
    'Workflow já iniciado para este material e centro. Aguarde finalização do cadastro'      TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.
ENDFORM.                " VALIDA_ENTRADA
*&---------------------------------------------------------------------*
*&      Form  F_RECUSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_RECUSA .
  DATA: WL_INPUT TYPE ZMMT0069.
  SELECT SINGLE * FROM ZMMT0069 INTO WL_INPUT WHERE NUREQ = WG_CADMAT-NUREQ.

  REFRESH: TG_EDITOR.
  IF OBG_DESCBOXR IS NOT INITIAL.
    CALL METHOD OBG_DESCBOXR->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.

    LOOP AT TG_EDITOR INTO WG_EDITOR.
      IF SY-TABIX EQ 1.
        WL_INPUT-RECUSA = WG_EDITOR-LINE.
      ELSEIF SY-TABIX GE 2.
        CONCATENATE WL_INPUT-RECUSA WG_EDITOR-LINE INTO WL_INPUT-RECUSA.
      ENDIF.
    ENDLOOP.
  ENDIF.
  WL_INPUT-CRIADO       = SY-UNAME.
  WL_INPUT-DT_CRIACAO   = SY-DATUM.
  WL_INPUT-HR_CRIACAO   = SY-UZEIT.
  WL_INPUT-RECUSA_FLAG  = 'X'.
  MODIFY ZMMT0069 FROM       WL_INPUT.
  "LOG
  MOVE-CORRESPONDING WL_INPUT TO WL_LOG.
  WL_LOG-DATA = SY-DATUM.
  WL_LOG-HORA = SY-UZEIT.
  WL_LOG-ACAO = 'cadastro recusado'.
  MODIFY ZMMT0069_LOG FROM       WL_LOG.
  COMMIT WORK.

  MESSAGE S836(SD) WITH TEXT-M01 WG_CADMAT-NUREQ TEXT-M02.
  PERFORM ENVIA_EMAIL USING 'R'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HOMOLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_HOMOLOG.

  REFRESH: TG_EDITOR.
  IF OBG_DESCBOXR IS NOT INITIAL.
    CALL METHOD OBG_DESCBOXR->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.
  ENDIF.

  IF TG_EDITOR[] IS NOT INITIAL.
    MESSAGE 'Limpe o texto de recusa para homologar' TYPE 'I'.
    EXIT.
  ENDIF.
  DATA: WL_INPUT TYPE ZMMT0069.
  SELECT SINGLE * FROM ZMMT0069 INTO WL_INPUT WHERE NUREQ = WG_CADMAT-NUREQ.

  "
  IF WG_CADMAT2-HOM_EPI = 'X'.
    WL_INPUT-HOMOLOG    = 'O'.
    WL_INPUT-EPI_CA     = WG_CADMAT-EPI_CA.
    WL_INPUT-EPI_PERI   = WG_CADMAT-EPI_PERI.
    WL_INPUT-EPI_VAL    = WG_CADMAT-EPI_VAL.
    WL_INPUT-CODAGREPI  = WG_CADMAT-CODAGREPI.
    WL_INPUT-HOMOLOGAF  = SY-UNAME.
    WL_INPUT-DT_HOMOLF  = SY-DATUM.
    WL_INPUT-HR_HOMOLF  = SY-UZEIT.
  ELSE.
    WL_INPUT-HOMOLOG    = 'S'.
    WL_INPUT-CODAGREPI  = WG_CADMAT-CODAGREPI.
    WL_INPUT-HOMOLOGA   = SY-UNAME.
    WL_INPUT-DT_HOMOL   = SY-DATUM.
    WL_INPUT-HR_HOMOL   = SY-UZEIT.
  ENDIF.
  MODIFY ZMMT0069 FROM       WL_INPUT.

  "LOG
  MOVE-CORRESPONDING WL_INPUT TO WL_LOG.
  WL_LOG-DATA = SY-DATUM.
  WL_LOG-HORA = SY-UZEIT.
  WL_LOG-ACAO = 'cadastro homologado'.
  MODIFY ZMMT0069_LOG FROM       WL_LOG.
  COMMIT WORK.

  MESSAGE S836(SD) WITH TEXT-M03 WG_CADMAT-NUREQ TEXT-M02.

  PERFORM ENVIA_EMAIL USING 'H'.

ENDFORM.
