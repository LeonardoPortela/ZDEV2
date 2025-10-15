*----------------------------------------------------------------------*
*                 A M A G G I   C O M M O D I T I E S                  *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMR109                                                 *
* Descrição  : Importação de Dados Classificação                       *
* Módulo     : MM                                                      *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Welgem Barbosa                         Data: 08/09/2016 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*

REPORT ZMMR109 NO STANDARD PAGE HEADING MESSAGE-ID SD.

TABLES: T001W, MARA.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF TYPE_DATA,
         CAMPO TYPE CHAR1024,
       END   OF TYPE_DATA,

       BEGIN OF TYPE_MCHB,
         MATNR TYPE MCHB-MATNR,
         WERKS TYPE MCHB-WERKS,
         LGORT TYPE MCHB-LGORT,
         CHARG TYPE MCHB-CHARG,
         OBJEK TYPE INOB-OBJEK,
       END   OF TYPE_MCHB,

       BEGIN OF TYPE_RMCLM,
         KLART TYPE TCLA-KLART,
         CUOBJ TYPE INOB-CUOBJ,
         CLINT TYPE KSSK-CLINT,
         CLASS TYPE KLAH-CLASS,
         VONDT TYPE KLAH-VONDT,
         BISDT TYPE KLAH-BISDT,
       END   OF TYPE_RMCLM,

       BEGIN OF TYPE_INOB,
         CUOBJ  TYPE INOB-CUOBJ,
         KLART  TYPE INOB-KLART,
         OBTAB  TYPE INOB-OBTAB,
         OBJEK  TYPE INOB-OBJEK,
         OBJEKK TYPE KSSK-OBJEK,
       END   OF TYPE_INOB,

       BEGIN OF TYPE_KSSK,
         OBJEK TYPE KSSK-OBJEK,
         MAFID TYPE KSSK-MAFID,
         KLART TYPE KSSK-KLART,
         CLINT TYPE KSSK-CLINT,
         ADZHL TYPE KSSK-ADZHL,
       END   OF TYPE_KSSK,

       BEGIN OF TYPE_KLAH,
         CLINT TYPE KLAH-CLINT,
         KLART TYPE KLAH-KLART,
         CLASS TYPE KLAH-CLASS,
         VONDT TYPE KLAH-VONDT,
         BISDT TYPE KLAH-BISDT,
       END   OF TYPE_KLAH,

       BEGIN OF TYPE_MSG,
         MATERIAL TYPE MCHB-MATNR,
         LOTE     TYPE MCHB-CHARG,
         CENTRO   TYPE MCHB-WERKS,
         TIPO     TYPE CHAR1,
         MESAGEM  TYPE BAPI_MSG,
       END   OF TYPE_MSG,

       BEGIN OF TYPE_KSML,
         CLINT TYPE KSML-CLINT,
         POSNR TYPE KSML-POSNR,
         ADZHL TYPE KSML-ADZHL,
         IMERK TYPE KSML-IMERK,
         KLART TYPE KSML-KLART,
       END   OF TYPE_KSML,

       BEGIN OF TYPE_CABNT,
         ATINN TYPE CABNT-ATINN,
         SPRAS TYPE CABNT-SPRAS,
         ADZHL TYPE CABNT-ADZHL,
         ATBEZ TYPE CABNT-ATBEZ,
         ATNAM TYPE CABN-ATNAM,
         ATFOR TYPE CABN-ATFOR,
       END   OF TYPE_CABNT,

       BEGIN OF TY_FILE,
         MATNR   TYPE MARA-MATNR,
         MATDE   TYPE MAKT-MAKTX,
         MARCA   TYPE CABNT-ATBEZ,
         ESPECIE TYPE ATWRT,
         LOTE    TYPE DFBATCH-CHARG,
       END OF TY_FILE.


*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: T_EXCEL        LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
      T_EXCEL2       LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
      T_DATA         TYPE TABLE OF TYPE_DATA,
      S_DATA         TYPE TYPE_DATA,
      T_FILE         TYPE TABLE OF ZMMI0001,
      IT_FILE        TYPE TABLE OF TY_FILE,
      WA_FILE        TYPE TY_FILE,
      T_MCHB         TYPE TABLE OF TYPE_MCHB,
      T_INOB         TYPE TABLE OF TYPE_INOB,
      T_KSSK         TYPE TABLE OF TYPE_KSSK,
      T_KSSK_AUX     TYPE TABLE OF TYPE_KSSK,
      T_KLAH         TYPE TABLE OF TYPE_KLAH,
      T_MSG          TYPE TABLE OF TYPE_MSG,
      T_KSML         TYPE TABLE OF TYPE_KSML,
      T_CABNT        TYPE TABLE OF TYPE_CABNT,
      ST_RMCLM       TYPE TYPE_RMCLM,
      GW_SAFRAFARDOS TYPE ZTSAFRAFARDOS.

DATA: VL_FILE  TYPE RLGRAP-FILENAME,
      VMSG(50).

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK A2 WITH FRAME TITLE TEXT-002.
PARAMETERS
  P_FILE TYPE FILE_NAME.
SELECTION-SCREEN END   OF BLOCK A2.

*----------------------------------------------------------------------*
*                          AT SELECTION-SCREEN                         *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
* Ajuda de Pesquisa Arquivo
  PERFORM Z_BUSCA_FILE.

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Upload File
  PERFORM Z_UPLOAD_FILE.

  CHECK NOT IT_FILE[] IS INITIAL.

* Seleciona Dados
  PERFORM: Z_SELECIONA_DADOS.

  T_KSSK_AUX[] = T_KSSK[].
  SORT T_KSSK_AUX BY CLINT.
  DELETE ADJACENT DUPLICATES FROM T_KSSK_AUX COMPARING CLINT.

  IF LINES( T_KSSK_AUX ) NE 1.
    PERFORM: Z_VERIFICA_CLASS.
  ELSE.
* Processa Dados
    PERFORM:    Z_PROCESSA_DADOS.
  ENDIF.

  IF NOT T_MSG[] IS INITIAL.
    CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
      TABLES
        TABLE    = T_MSG
      EXCEPTIONS
        FB_ERROR = 1
        OTHERS   = 2.

    IF NOT SY-SUBRC IS INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleciona Dados                           *
*----------------------------------------------------------------------*
FORM Z_SELECIONA_DADOS.
  REFRESH T_MSG.

* Seleciona MCHB
  PERFORM: Z_SELECIONA_MCHB,
* Seleciona Dados Características
           Z_SELECIONA_CARAC.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                             Processa Dados                           *
*----------------------------------------------------------------------*
FORM Z_PROCESSA_DADOS.
  DATA: SL_FILE TYPE TY_FILE,
        SL_MCHB TYPE TYPE_MCHB,
        LOCAL   TYPE STRING,
        SL_MSG  TYPE TYPE_MSG.

  SORT: T_INOB  BY OBJEK ASCENDING,
        T_KSSK  BY OBJEK ASCENDING,
        T_KLAH  BY CLINT ASCENDING,
        T_CABNT BY ATNAM ASCENDING.

  LOOP AT IT_FILE INTO SL_FILE.

    LOOP AT T_MCHB INTO SL_MCHB WHERE CHARG EQ SL_FILE-LOTE
                                  AND MATNR EQ SL_FILE-MATNR.

*     Executa Bapi MSC2N

      PERFORM Z_EXECUTA_MSC2N USING SL_FILE
                                       SL_MCHB.

      CLEAR SL_MCHB.
    ENDLOOP.

    CLEAR SL_MCHB.
    READ TABLE T_MCHB INTO SL_MCHB WITH KEY MATNR = SL_FILE-MATNR
                                            CHARG = SL_FILE-LOTE.
    IF NOT SY-SUBRC IS INITIAL.

      MOVE SL_FILE-MATNR      TO SL_MSG-MATERIAL.
      MOVE SL_FILE-LOTE       TO SL_MSG-LOTE.
      MOVE SL_MCHB-WERKS      TO SL_MSG-CENTRO.
      MOVE 'Z'                TO SL_MSG-TIPO.
      MOVE 'Lote não Existe'  TO SL_MSG-MESAGEM.

      APPEND SL_MSG TO T_MSG.
      CLEAR SL_MSG.

    ENDIF.

    CLEAR SL_FILE.
  ENDLOOP.

  PERFORM SAIDA_TXT CHANGING LOCAL.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME              = LOCAL
      WRITE_FIELD_SEPARATOR = '#'
    TABLES
      DATA_TAB              = T_MSG.

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTA_MSC2N                                          *
*&---------------------------------------------------------------------*
*                         Executa Bapi MSC2N                           *
*----------------------------------------------------------------------*
FORM Z_EXECUTA_MSC2N USING P_FILE TYPE TY_FILE
                           P_MCHB TYPE TYPE_MCHB.

  DATA: VL_NUM   TYPE BAPI1003_KEY-CLASSNUM,
        VL_TYPE  TYPE BAPI1003_KEY-CLASSTYPE,
        VL_TABLE TYPE BAPI1003_KEY-OBJECTTABLE,
        VL_KEY   TYPE BAPI1003_KEY-OBJECT,
        SL_KEYS  TYPE BAPI1003_OBJECT_KEYS,
        SL_INOB  TYPE TYPE_INOB,
        SL_KSSK  TYPE TYPE_KSSK,
        SL_KLAH  TYPE TYPE_KLAH,
        TL_TABLE TYPE TABLE OF BAPI1003_OBJECT_KEYS,
        TL_RET   TYPE TABLE OF BAPIRET2,
        TL_NUM   TYPE TABLE OF BAPI1003_ALLOC_VALUES_NUM,
        TL_CHAR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
        TL_CURR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CURR,
        SAIDA    TYPE CHAR50.

  REFRESH: TL_TABLE, TL_RET.
  CLEAR: SL_INOB, SL_KSSK, SL_KLAH.

  SL_KEYS-KEY_FIELD = 'MATNR'.
*---> 04/07/2023 - Migração S4 - AF
* SL_KEYS-VALUE_INT = P_MCHB-MATNR.
  SL_KEYS-VALUE_INT = CONV #( P_MCHB-MATNR ).
*---> 04/07/2023 - Migração S4 - AF
  APPEND SL_KEYS TO TL_TABLE.

  SL_KEYS-KEY_FIELD = 'CHARG'.
  SL_KEYS-VALUE_INT = P_MCHB-CHARG.
  APPEND SL_KEYS TO TL_TABLE.

  VL_TABLE = 'MCH1'.

  CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
    EXPORTING
      OBJECTTABLE    = VL_TABLE
    IMPORTING
      OBJECTKEY_CONC = VL_KEY
    TABLES
      OBJECTKEYTABLE = TL_TABLE
      RETURN         = TL_RET.

  CHECK SY-SUBRC IS INITIAL.

  READ TABLE T_INOB INTO SL_INOB WITH KEY OBJEK = P_MCHB-OBJEK   BINARY SEARCH.

  READ TABLE T_KSSK INTO SL_KSSK WITH KEY OBJEK = SL_INOB-OBJEKK BINARY SEARCH.

  READ TABLE T_KLAH INTO SL_KLAH WITH KEY CLINT = SL_KSSK-CLINT  BINARY SEARCH.

  VL_NUM  = SL_KLAH-CLASS.
  VL_TYPE = ST_RMCLM-KLART.

* Preenche Características
  PERFORM Z_PREENCHE_CARAC TABLES TL_CHAR
                                  TL_NUM
                            USING P_FILE
                                  VL_KEY
                                  VL_TABLE
                                  VL_NUM
                                  VL_TYPE.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE'"#EC CI_USAGE_OK[2438131]
    EXPORTING
      OBJECTKEY          = VL_KEY
      OBJECTTABLE        = VL_TABLE
      CLASSNUM           = VL_NUM
      CLASSTYPE          = VL_TYPE
    TABLES
      ALLOCVALUESNUMNEW  = TL_NUM
      ALLOCVALUESCHARNEW = TL_CHAR
      ALLOCVALUESCURRNEW = TL_CURR
      RETURN             = TL_RET.

  DELETE TL_RET WHERE ( TYPE NE 'E' AND TYPE NE 'S' ).
* Retorna MSG
  PERFORM Z_RETORNA_MSG TABLES TL_RET
                         USING P_MCHB-MATNR
                               P_MCHB-CHARG
                               P_MCHB-WERKS.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.

  CLEAR SAIDA.
  CONCATENATE 'Finalizando... ' VL_KEY INTO SAIDA SEPARATED BY SPACE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = SAIDA.

  WAIT UP TO 2 SECONDS.

ENDFORM.                    " Z_EXECUTA_MSC2N

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CARAC                                         *
*&---------------------------------------------------------------------*
*                      Preenche Características                        *
*----------------------------------------------------------------------*
FORM Z_PREENCHE_CARAC TABLES P_CHAR  STRUCTURE BAPI1003_ALLOC_VALUES_CHAR
                             P_TNUM  STRUCTURE BAPI1003_ALLOC_VALUES_NUM
                       USING P_FILE  TYPE TY_FILE
                             P_KEY   TYPE BAPI1003_KEY-OBJECT
                             P_TABLE TYPE BAPI1003_KEY-OBJECTTABLE
                             P_NUM   TYPE BAPI1003_KEY-CLASSNUM
                             P_TYPE  TYPE BAPI1003_KEY-CLASSTYPE.

  DATA: TL_CAT   TYPE LVC_T_FCAT,
        SL_CAT   TYPE LVC_S_FCAT,
        VL_CAMPO TYPE CHAR30,
        SL_CHAR  TYPE BAPI1003_ALLOC_VALUES_CHAR,
        SL_NUM   TYPE BAPI1003_ALLOC_VALUES_NUM,
        SL_AUX   TYPE BAPI1003_ALLOC_VALUES_CHAR,
        SL_CABNT TYPE TYPE_CABNT,
        TL_AUX   TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
        TL_NUM   TYPE TABLE OF BAPI1003_ALLOC_VALUES_NUM,
        TL_CHAR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
        TL_CURR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CURR,
        TL_RET   TYPE TABLE OF BAPIRET2.

  FIELD-SYMBOLS <CAMPO> TYPE ANY.

  REFRESH: P_CHAR, P_TNUM.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'"#EC CI_USAGE_OK[2438131]
    EXPORTING
      OBJECTKEY       = P_KEY
      OBJECTTABLE     = P_TABLE
      CLASSNUM        = P_NUM
      CLASSTYPE       = P_TYPE
    TABLES
      ALLOCVALUESNUM  = TL_NUM
      ALLOCVALUESCHAR = TL_CHAR
      ALLOCVALUESCURR = TL_CURR
      RETURN          = TL_RET.

  SORT: TL_NUM  BY CHARACT ASCENDING,
        TL_CHAR BY CHARACT ASCENDING.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZMM_FCAT_EXCEL'
    CHANGING
      CT_FIELDCAT            = TL_CAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT TL_CAT INTO SL_CAT.
    CLEAR: VL_CAMPO, SL_AUX.

    CONCATENATE 'P_FILE' SL_CAT-FIELDNAME INTO VL_CAMPO SEPARATED BY '-'.

    ASSIGN (VL_CAMPO) TO <CAMPO>.

    CHECK <CAMPO> IS ASSIGNED.

    SL_AUX-CHARACT_DESCR  = SL_CAT-FIELDNAME.
    SL_AUX-VALUE_CHAR     = <CAMPO>.

    APPEND SL_AUX TO TL_AUX.

    CLEAR SL_CAT.
    UNASSIGN <CAMPO>.
  ENDLOOP.

  SORT TL_AUX BY CHARACT_DESCR ASCENDING.

  LOOP AT T_CABNT INTO SL_CABNT.
    CLEAR: P_CHAR, P_TNUM.

    READ TABLE TL_AUX INTO SL_AUX WITH KEY CHARACT_DESCR = SL_CABNT-ATBEZ BINARY SEARCH.
    IF NOT SL_AUX-VALUE_CHAR IS INITIAL.
      CASE SL_CABNT-ATFOR.
        WHEN 'CHAR'.
          P_CHAR-CHARACT     = SL_CABNT-ATNAM.
          P_CHAR-VALUE_CHAR  = SL_AUX-VALUE_CHAR.
          APPEND P_CHAR.
        WHEN 'NUM'.
          P_TNUM-CHARACT     = SL_CABNT-ATNAM.
          P_TNUM-VALUE_FROM  = SL_AUX-VALUE_CHAR.
          APPEND P_TNUM.
      ENDCASE.
    ELSE.
      CASE SL_CABNT-ATBEZ.

        WHEN 'Variedade' OR 'Talhao'
          OR 'Safra' OR 'Periodo'.    "ADD - 21.06.2013

          READ TABLE TL_CHAR INTO SL_CHAR WITH KEY CHARACT = SL_CABNT-ATNAM BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            P_CHAR-CHARACT     = SL_CABNT-ATNAM.
            P_CHAR-VALUE_CHAR  = SL_CHAR-VALUE_CHAR.
            APPEND P_CHAR.
          ENDIF.
      ENDCASE.
    ENDIF.

    CLEAR: SL_CABNT, SL_AUX, SL_NUM, SL_CHAR.
  ENDLOOP.

ENDFORM.                    " Z_PREENCHE_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MCHB                                         *
*&---------------------------------------------------------------------*
*                            Seleciona MCHB                            *
*----------------------------------------------------------------------*
FORM Z_SELECIONA_MCHB.

  SELECT MATNR WERKS LGORT CHARG
  FROM MCHB
    INTO TABLE T_MCHB
    FOR ALL ENTRIES IN IT_FILE
  WHERE MATNR EQ IT_FILE-MATNR
    AND CHARG EQ IT_FILE-LOTE.

  SORT T_MCHB BY CHARG ASCENDING
                 WERKS ASCENDING
                 MATNR ASCENDING.

  DELETE ADJACENT DUPLICATES FROM T_MCHB COMPARING CHARG WERKS MATNR.

  IF T_MCHB[] IS INITIAL.
    MESSAGE I836 WITH TEXT-005.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Move Matnr p/ Objek
  PERFORM Z_MOVE_MATNR_OBJEK.

ENDFORM.                    " Z_SELECIONA_MCHB

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_MATNR_OBJEK                                       *
*&---------------------------------------------------------------------*
*                          Move Matnr p/ Objek                         *
*----------------------------------------------------------------------*
FORM Z_MOVE_MATNR_OBJEK.

  DATA: SL_MCHB  TYPE TYPE_MCHB,
        VL_INDEX TYPE I.

  LOOP AT T_MCHB INTO SL_MCHB.
    VL_INDEX = SY-TABIX.

*---> 04/07/2023 - Migração S4 - AF
*   SL_MCHB-OBJEK = SL_MCHB-MATNR.
    SL_MCHB-OBJEK = CONV #( SL_MCHB-MATNR ).
*---> 04/07/2023 - Migração S4 - AF

    MODIFY T_MCHB FROM SL_MCHB INDEX VL_INDEX TRANSPORTING OBJEK.

    CLEAR SL_MCHB.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_MATNR_OBJEK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_CARAC                                        *
*&---------------------------------------------------------------------*
*                     Seleciona Dados Características                  *
*----------------------------------------------------------------------*
FORM Z_SELECIONA_CARAC.
  DATA: TL_MCHB TYPE TABLE OF TYPE_MCHB,
        TL_KSSK TYPE TABLE OF TYPE_KSSK.

  REFRESH: T_INOB, T_KSSK, T_KLAH, T_KSML, T_CABNT.
  CLEAR ST_RMCLM.

  CHECK NOT T_MCHB[] IS INITIAL.
  TL_MCHB[] = T_MCHB[].
  SORT TL_MCHB BY OBJEK ASCENDING.
  DELETE ADJACENT DUPLICATES FROM TL_MCHB COMPARING OBJEK.

  SELECT SINGLE A~KLART FROM TCLA AS A
    INNER JOIN TCLAO AS B ON A~KLART EQ B~KLART
    INTO ST_RMCLM-KLART
  WHERE A~OBTAB    EQ 'MCHA'
    AND A~INTKLART EQ SPACE
    AND A~MULTOBJ  EQ 'X'
    AND B~OBTAB    EQ 'MCH1'.

  SELECT CUOBJ KLART OBTAB OBJEK
  FROM INOB
    INTO TABLE T_INOB
    FOR ALL ENTRIES IN TL_MCHB
   WHERE KLART EQ ST_RMCLM-KLART
     AND OBTAB EQ 'MARA'
     AND OBJEK EQ TL_MCHB-OBJEK.

  CHECK NOT T_INOB[] IS INITIAL.
* Move Cuobj p/ Objekk
  PERFORM Z_MOVE_CUOBJ_OBJEKK.

  SELECT OBJEK MAFID KLART CLINT ADZHL
  FROM KSSK
    INTO TABLE T_KSSK
    FOR ALL ENTRIES IN T_INOB
  WHERE OBJEK EQ T_INOB-OBJEKK
    AND MAFID EQ 'O'
    AND KLART EQ ST_RMCLM-KLART.

  CHECK NOT T_KSSK[] IS INITIAL.
  TL_KSSK[] = T_KSSK[].
  SORT TL_KSSK BY CLINT ASCENDING.
  DELETE ADJACENT DUPLICATES FROM TL_KSSK COMPARING CLINT.

  SELECT CLINT KLART CLASS VONDT BISDT
  FROM KLAH
    INTO TABLE T_KLAH
    FOR ALL ENTRIES IN TL_KSSK
   WHERE CLINT EQ TL_KSSK-CLINT.

  SELECT CLINT POSNR ADZHL IMERK KLART
  FROM KSML
    INTO TABLE T_KSML
    FOR ALL ENTRIES IN TL_KSSK
  WHERE CLINT EQ TL_KSSK-CLINT
    AND KLART EQ ST_RMCLM-KLART.

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

ENDFORM.                    " Z_SELECIONA_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_CUOBJ_OBJEKK                                      *
*&---------------------------------------------------------------------*
*                          Move Cuobj p/ Objekk                        *
*----------------------------------------------------------------------*
FORM Z_MOVE_CUOBJ_OBJEKK.
  DATA: SL_INOB  TYPE TYPE_INOB,
        VL_INDEX TYPE I.

  LOOP AT T_INOB INTO SL_INOB.
    VL_INDEX = SY-TABIX.

    SL_INOB-OBJEKK = SL_INOB-CUOBJ.

    MODIFY T_INOB FROM SL_INOB INDEX VL_INDEX TRANSPORTING OBJEKK.

    CLEAR SL_INOB.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_CUOBJ_OBJEKK

*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG                                            *
*&---------------------------------------------------------------------*
*                               Retorna MSG                            *
*----------------------------------------------------------------------*
FORM Z_RETORNA_MSG TABLES P_RET   STRUCTURE BAPIRET2
                    USING P_MATNR TYPE MCHB-MATNR
                          P_CHARG TYPE MCHB-CHARG
                          P_WERKS TYPE MCHB-WERKS.

  DATA: SL_RET TYPE BAPIRET2,
        SL_MSG TYPE TYPE_MSG.

  LOOP AT P_RET INTO SL_RET.
    SL_MSG-MATERIAL = P_MATNR.
    SL_MSG-LOTE     = P_CHARG.
    SL_MSG-CENTRO   = P_WERKS.
    SL_MSG-TIPO     = SL_RET-TYPE.
    SL_MSG-MESAGEM  = SL_RET-MESSAGE.

    APPEND SL_MSG TO T_MSG.

    CLEAR: SL_RET, SL_MSG.
  ENDLOOP.

ENDFORM.                    " Z_RETORNA_MSG



*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG_2                                          *
*&---------------------------------------------------------------------*
*                               Retorna MSG                            *
*----------------------------------------------------------------------*
FORM Z_RETORNA_MSG_2 USING P_TEXT  TYPE C
                           P_CHARG TYPE MCHB-CHARG.

  DATA SL_MSG TYPE TYPE_MSG.

  SL_MSG-LOTE     = P_CHARG.
  SL_MSG-CENTRO   = ''.
  SL_MSG-TIPO     = 'E'.
  SL_MSG-MESAGEM  = P_TEXT.

  APPEND SL_MSG TO T_MSG.

ENDFORM.                    " Z_RETORNA_MSG_2
*&---------------------------------------------------------------------*
*&      Form  Z_UPLOAD_FILE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_UPLOAD_FILE.

  CHECK NOT P_FILE IS INITIAL.

  DATA: VL_FILE  TYPE RLGRAP-FILENAME,
        VMSG(50).

  REFRESH: T_EXCEL, IT_FILE.

  MOVE P_FILE TO VL_FILE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = VL_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 5
      I_END_ROW               = 10000
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Atualizando Dados'.

  IF T_EXCEL[] IS NOT INITIAL.
    T_EXCEL2[] = T_EXCEL[].

    SORT T_EXCEL2 BY ROW COL.

    LOOP AT T_EXCEL.
      IF T_EXCEL-ROW = T_EXCEL2-ROW.
        CONTINUE.
      ENDIF.

      LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.

        CASE T_EXCEL2-COL.
          WHEN 1. MOVE T_EXCEL2-VALUE TO WA_FILE-MATNR.
          WHEN 2. MOVE T_EXCEL2-VALUE TO WA_FILE-MATDE.
          WHEN 3. MOVE T_EXCEL2-VALUE TO WA_FILE-MARCA.
          WHEN 4. MOVE T_EXCEL2-VALUE TO WA_FILE-ESPECIE.
          WHEN 5. MOVE T_EXCEL2-VALUE TO WA_FILE-LOTE.
        ENDCASE.

      ENDLOOP.

      CONCATENATE 'Linha ' T_EXCEL-ROW INTO VMSG SEPARATED BY SPACE.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          TEXT = VMSG.

      APPEND WA_FILE TO IT_FILE.

    ENDLOOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_BUSCA_FILE .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = 'C:\'
      MASK             = '*.XLS'
      MODE             = 'O'
      TITLE            = 'Busca de Arquivo'
    IMPORTING
      FILENAME         = VL_FILE
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.

  MOVE VL_FILE TO P_FILE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAIDA_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LOCAL  text
*----------------------------------------------------------------------*
FORM SAIDA_TXT  CHANGING P_LOCAL.

  DATA: LEN1  TYPE I,
        OFST1 TYPE I,
        OFST2 TYPE I,
        STR_C TYPE STRING.

  LEN1 = STRLEN( P_FILE ).

  DO.

    IF OFST1 = LEN1.
      EXIT.
    ENDIF.

    IF P_FILE+OFST1(1) CO '\'.
      OFST1 = OFST1 + 1.
      OFST2 = OFST1.
*    ELSE.
*      CONCATENATE STR_C P_FILE+OFST1(1) INTO STR_C.
    ENDIF.

    OFST1 = OFST1 + 1.

  ENDDO.

  CONCATENATE P_FILE(OFST2) 'IMPORT-' SY-UNAME '-' SY-DATUM '-' SY-UZEIT '.TXT'  INTO P_LOCAL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_CLASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_VERIFICA_CLASS .

  DATA: SL_MCHB TYPE TYPE_MCHB,
        SL_INOB TYPE TYPE_INOB,
        SL_KSSK TYPE TYPE_KSSK,
        SL_KLAH TYPE TYPE_KLAH,
        SL_MSG  TYPE TYPE_MSG.

  LOOP AT IT_FILE INTO WA_FILE.

    READ TABLE T_MCHB INTO SL_MCHB WITH KEY MATNR = WA_FILE-MATNR
                                            CHARG = WA_FILE-LOTE.
    IF NOT SY-SUBRC IS INITIAL.

      MOVE WA_FILE-MATNR      TO SL_MSG-MATERIAL.
      MOVE WA_FILE-LOTE       TO SL_MSG-LOTE.
      MOVE SL_MCHB-WERKS      TO SL_MSG-CENTRO.
      MOVE 'Z'                TO SL_MSG-TIPO.
      MOVE 'Lote não existe!' TO SL_MSG-MESAGEM.

      APPEND SL_MSG TO T_MSG.
      CLEAR SL_MSG.

    ELSE.

      READ TABLE T_INOB INTO SL_INOB WITH KEY KLART = ST_RMCLM-KLART
                                              OBTAB = 'MARA'
                                              OBJEK = SL_MCHB-OBJEK.
      READ TABLE T_KSSK INTO SL_KSSK WITH KEY OBJEK = SL_INOB-OBJEKK
                                              MAFID = 'O'
                                              KLART = ST_RMCLM-KLART.
      READ TABLE T_KLAH INTO SL_KLAH WITH KEY CLINT = SL_KSSK-CLINT.


      MOVE WA_FILE-MATNR      TO SL_MSG-MATERIAL.
      MOVE WA_FILE-LOTE       TO SL_MSG-LOTE.
      MOVE SL_MCHB-WERKS      TO SL_MSG-CENTRO.
      MOVE 'Z'                TO SL_MSG-TIPO.

      CONCATENATE 'Class:' SL_KLAH-CLASS INTO SL_MSG-MESAGEM SEPARATED BY SPACE.

      APPEND SL_MSG TO T_MSG.
      CLEAR SL_MSG.

    ENDIF.

    CLEAR: WA_FILE, SL_MCHB, SL_INOB, SL_KSSK, SL_KLAH.
  ENDLOOP.


ENDFORM.
