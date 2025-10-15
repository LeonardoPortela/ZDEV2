*&---------------------------------------------------------------------*
*& Report  ZFIR073
*& Geração Arquivo TXT - Contabilidade Amaggi Serviços Financeiros
*&---------------------------------------------------------------------*

REPORT ZFIR073.

TYPES: TY_R_BUKRS TYPE RANGE OF BSIS-BUKRS,
       TY_R_GJAHR TYPE RANGE OF BSIS-GJAHR,
       TY_R_BELNR TYPE RANGE OF BSIS-BELNR,
       TY_R_KOSTL TYPE RANGE OF BSIS-KOSTL,
       TY_R_BUZEI TYPE RANGE OF BSIS-BUZEI.

DATA: V_MES                   TYPE MONAT,
      V_ANO                   TYPE GJAHR,

      IT_HEADER               TYPE TABLE OF ZFIE_ARQ_ASF_CABECALHO,
      IT_DETALHE              TYPE TABLE OF ZFIE_ARQ_ASF_DETALHE,
      IT_CONTAS               TYPE TABLE OF ZFIT0153,
      CTL_CCCONTAINER_PICTURE TYPE REF TO CL_GUI_CONTAINER,
      SPLITTER                TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      PICTURE                 TYPE REF TO CL_GUI_PICTURE,

      CONTAINER_CONTAS        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV_CONTAS              TYPE REF TO CL_GUI_ALV_GRID.



CLASS CL_ARQ DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS GET_PIC_TAB IMPORTING MIME_URL TYPE CSEQUENCE EXPORTING PIC_TAB  TYPE STANDARD TABLE.

    METHODS:

      GET_DADOS IMPORTING I_MONAT   TYPE CHAR2
                          I_YEAR    TYPE GJAHR
                          I_BUKRS   TYPE BUKRS
                          I_USUARIO TYPE CHAR060,

      MONTA_ARQUIVO,

      GET_CONTAS,

      SET_CONTAS,

      GRAVA_ARQUIVO.

ENDCLASS.


CLASS CL_ARQ IMPLEMENTATION.

  METHOD GET_DADOS.

    DATA: VL_FILENAME TYPE STRING,
          VL_PATH     TYPE STRING,
          VL_FULLPATH TYPE STRING,
          V_USUARIO   TYPE CHAR060,
          IT_FILE     TYPE TABLE OF STRING,
          W_FILE      LIKE LINE OF IT_FILE.

    DATA: V_VALOR           TYPE N LENGTH 17,
          V_SEQUENCIAL      TYPE N LENGTH 3,
          V_SEQUENCIAL_ITEM TYPE N LENGTH 15,
          V_DS_HIST         TYPE C LENGTH 40,
          V_DS_CHIST        TYPE C LENGTH 40,
          V_CCUSTO          TYPE C LENGTH 5.

    V_USUARIO = TO_LOWER( I_USUARIO ).

    DATA(V_LENGHT) = STRLEN( V_USUARIO ).

    WHILE V_LENGHT < 60.
      V_USUARIO = |{ V_USUARIO } |.
      V_LENGHT = V_LENGHT + 1.
    ENDWHILE.

    ME->GET_CONTAS( ).

    SELECT
      FG~RYEAR,
      FG~DOCNR,
      FG~RBUKRS,
      FG~RACCT,
      FG~RCNTR,
      FG~KOKRS,
      FG~TSL,
      FG~HSL,
      FG~POPER,
      FG~BUDAT,
      FG~BELNR,
      FG~BUZEI,
      FG~BSTAT,
      FG~USNAM
    FROM FAGLFLEXA AS FG
    INTO TABLE @DATA(IT_FAGLFLEXA)
    WHERE FG~RYEAR  = @I_YEAR
      AND FG~RBUKRS = @I_BUKRS
      AND FG~POPER  = @I_MONAT.

    IF SY-SUBRC = 0.

      SORT IT_FAGLFLEXA[] BY RYEAR DOCNR ASCENDING.

      DATA(R_BUKRS) = VALUE TY_R_BUKRS(
        FOR LS_BUKRS IN IT_FAGLFLEXA LET S = 'I' O = 'EQ'
          IN SIGN     = S OPTION   = O ( LOW = LS_BUKRS-RBUKRS ) ).

      DATA(R_GJAHR) = VALUE TY_R_GJAHR(
        FOR LS_GJAHR IN IT_FAGLFLEXA LET S = 'I' O = 'EQ'
          IN SIGN     = S OPTION   = O ( LOW = LS_GJAHR-RYEAR ) ).

      DATA(R_BELNR) = VALUE TY_R_BELNR(
        FOR LS_BELNR IN IT_FAGLFLEXA LET S = 'I' O = 'EQ'
          IN SIGN     = S OPTION   = O ( LOW = LS_BELNR-BELNR ) ).

      DATA(R_KOSTL) = VALUE TY_R_KOSTL(
        FOR LS_KOSTL IN IT_FAGLFLEXA LET S = 'I' O = 'EQ'
          IN SIGN     = S OPTION   = O ( LOW = LS_KOSTL-RCNTR ) ).

      DATA(R_BUZEI) = VALUE TY_R_BUZEI(
        FOR LS_BUZEI IN IT_FAGLFLEXA LET S = 'I' O = 'EQ'
          IN SIGN     = S OPTION   = O ( LOW = LS_BUZEI-BUZEI ) ).

      SORT: R_BUKRS[], R_GJAHR[], R_BELNR[] BY LOW ASCENDING.

      SORT R_BUKRS[] BY LOW ASCENDING. DELETE ADJACENT DUPLICATES FROM R_BUKRS[] COMPARING LOW.
      SORT R_GJAHR[] BY LOW ASCENDING. DELETE ADJACENT DUPLICATES FROM R_GJAHR[] COMPARING LOW.
      SORT R_BELNR[] BY LOW ASCENDING. DELETE ADJACENT DUPLICATES FROM R_BELNR[] COMPARING LOW.
      SORT R_KOSTL[] BY LOW ASCENDING. DELETE ADJACENT DUPLICATES FROM R_KOSTL[] COMPARING LOW.
*      SORT R_BUZEI[] BY LOW ASCENDING. DELETE ADJACENT DUPLICATES FROM R_BUZEI[] COMPARING LOW.

      SELECT BS~BUKRS,
             BS~HKONT,
             BS~AUGDT,
             BS~AUGBL,
             BS~ZUONR,
             BS~GJAHR,
             BS~BELNR,
             BS~BUZEI,
             BS~BUDAT,
             BS~SGTXT
        FROM BSIS AS BS
        INTO TABLE @DATA(IT_BSIS)
        WHERE BS~BUKRS IN @R_BUKRS[]
          AND BS~BELNR IN @R_BELNR[]
          AND BS~GJAHR IN @R_GJAHR[]
          AND BS~BUZEI IN @R_BUZEI[].

*      IF SY-SUBRC = 0.

      SORT IT_FAGLFLEXA[] BY RYEAR DOCNR RBUKRS BUDAT BELNR BUZEI ASCENDING.
      DELETE ADJACENT DUPLICATES FROM IT_FAGLFLEXA[] COMPARING RYEAR DOCNR RBUKRS BUDAT BELNR BUZEI.
      SORT IT_BSIS[] BY BUDAT ASCENDING.

      SELECT CS~KOKRS, CS~KOSTL, CS~ANRED FROM CSKS AS CS
        INTO TABLE @DATA(T_ANRED)
        WHERE CS~KOSTL IN @R_KOSTL.


      LOOP AT IT_FAGLFLEXA[] INTO DATA(W_FAGLFLEXA)
        GROUP BY ( BUDAT = W_FAGLFLEXA-BUDAT
                   SIZE  = GROUP SIZE
                   INDEX = GROUP INDEX ) ASCENDING
                  REFERENCE INTO DATA(GROUP_FAGLFLEXA).

        LOOP AT GROUP GROUP_FAGLFLEXA ASSIGNING FIELD-SYMBOL(<W_GROUP>). " WHERE HSL > 0.

          DATA(W_HEADER) = VALUE ZFIE_ARQ_ASF_CABECALHO(
              NR_INST   = '01204'
              NR_AGEN   = '00019'
              CD_REG    = '019'
              AM_MOVTO  = |{ <W_GROUP>-RYEAR+2(2) }{ <W_GROUP>-POPER+1(2) }|
              DD_MOVTO  = |{ <W_GROUP>-BUDAT+6(2) }|
              NR_LOTE   = '43'
              NR_SEQ    = '000000000000000'
              TP_DEBCRE = ' '
              NR_SEQUEN = '00000'
              VL_TDEB   = COND #( WHEN <W_GROUP>-HSL < 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              VL_TCRED  = COND #( WHEN <W_GROUP>-HSL > 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              VL_TDEBM  = COND #( WHEN <W_GROUP>-HSL < 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              VL_RCREDM = COND #( WHEN <W_GROUP>-HSL > 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              ST_LOTE   = '1'
              TP_OPER   = ' '
              TP_LANC   = '019'
              QT_TLANC  = '00001'
              QT_CLANC  = '00001'
              VL_CDEB   = COND #( WHEN <W_GROUP>-HSL < 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              VL_CCRED  = COND #( WHEN <W_GROUP>-HSL > 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              VL_LOTE   = COND #( WHEN <W_GROUP>-HSL > 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              VL_LOTEM  = COND #( WHEN <W_GROUP>-HSL > 0 THEN <W_GROUP>-HSL ELSE '00000000000000000' )
              DT_PROC   = |{ SY-DATUM+6(2) }{ SY-DATUM+4(2) }{ SY-DATUM+2(2) }|
              CD_CORRES = ' '
              DS_FILLER = '                                                                                                          '
          ).

          COLLECT W_HEADER INTO IT_HEADER[].

          READ TABLE IT_BSIS[] INTO DATA(W_BSIS) WITH KEY BUKRS = <W_GROUP>-RBUKRS
                                                          GJAHR = <W_GROUP>-RYEAR
                                                          BELNR = <W_GROUP>-BELNR
                                                          BUZEI = <W_GROUP>-BUZEI.

          V_VALOR = ( <W_GROUP>-HSL * 100 ).

          V_DS_HIST = W_BSIS-SGTXT.
          DATA(V_LENG) = STRLEN( V_DS_HIST ).
          WHILE V_LENG < 40.
            V_DS_HIST = |{ V_DS_HIST } |.
            V_LENG = V_LENG + 1.
          ENDWHILE.

          CLEAR: V_LENG.
          V_DS_CHIST = |{ <W_GROUP>-BELNR }A|.
          V_LENG = STRLEN( V_DS_CHIST ).
          WHILE V_LENG < 41.
            V_DS_CHIST = |{ V_DS_CHIST } |.
            V_LENG = V_LENG + 1.
            REPLACE ALL OCCURRENCES OF 'A' IN V_DS_CHIST WITH ''.
          ENDWHILE.

          CLEAR: V_LENG.

          TRY.
              DATA(V_CONTA) = IT_CONTAS[ RACCT = <W_GROUP>-RACCT ]-CONTA_BANCO.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

          IF V_CONTA IS NOT INITIAL.
            <W_GROUP>-RACCT = V_CONTA.
          ENDIF.

          READ TABLE T_ANRED[] INTO DATA(W_ANRED)
            WITH KEY KOSTL = <W_GROUP>-RCNTR
                     KOKRS = <W_GROUP>-KOKRS.

          IF SY-SUBRC = 0.
            V_CCUSTO = W_ANRED-ANRED(5).
            DATA(V_LEN_CUSTO) = STRLEN( W_ANRED-ANRED ).
            WHILE V_LEN_CUSTO < 5.
              V_CCUSTO = |0{ V_CCUSTO }|.
              V_LEN_CUSTO = V_LEN_CUSTO + 1.
            ENDWHILE.
          ENDIF.


          DATA(W_DETALHE) = VALUE ZFIE_ARQ_ASF_DETALHE(
              NR_INST   = W_HEADER-NR_INST
              NR_AGEN   = W_HEADER-NR_AGEN
              CD_REG    = W_HEADER-CD_REG
              AM_MOVTO  = W_HEADER-AM_MOVTO
              DD_MOVTO  = W_HEADER-DD_MOVTO
              NR_LOTE   = W_HEADER-NR_LOTE
              NR_SEQ    = '000000000000000'
              TP_DEBCRE = ' '
              TP_MOVTO  = '000'
              CD_LANC   = '019'
              CT_CONTCP = '000000000000000'
              NR_CRES   = '00000'
              CD_HIST   = '00000'
              VL_LANC   = V_VALOR "<W_GROUP>-HSL
              DS_HIST   = V_DS_HIST
              DS_CHIST  = V_DS_CHIST
              CD_COMPDS = COND #( WHEN <W_GROUP>-BELNR IS NOT INITIAL THEN 'S' ELSE 'N' )
              DS_ESPAC1 = ' '
              NR_DOCUMT = '0000000'
              NR_ZEROS1 = '00000'
              NR_ZEROS2 = '00'
              NR_ZEROS3 = '0'
              VL_MOEDUS = '00000000000000000'
              CT_CONT   = '000000000000000'
              NR_MOED   = '00000'
              TP_CONT   = 'I'
              CT_NINT   = <W_GROUP>-RACCT
              CT_NINTCP = <W_GROUP>-RACCT
              DS_ESPAC2 = ' '
              NR_ZEROS4 = '000000'
              NR_ZEROS5 = '0'
              DS_ESPAC3 = ' '
              NR_CRCRE  = V_CCUSTO                          "'00000'
              NR_MOEDUS = '00000'
              VL_MOEDA  = '00000000000000000'
              NR_CRDEB9 = COND #( WHEN <W_GROUP>-TSL > 0 THEN <W_GROUP>-RACCT ELSE '000000000' )
              NR_CRCRE9 = COND #( WHEN <W_GROUP>-TSL < 0 THEN <W_GROUP>-RACCT ELSE '000000000' )
              CD_LANCCR = '000'
              NR_RUBRIC = '     '
              NR_CRROPD = '000000000'
              NR_CRROPC = '000000000'
              TP_LOTE   = 'A'
              NM_LOGIN  = V_USUARIO   ).

          APPEND W_DETALHE TO IT_DETALHE[].

          CLEAR: V_VALOR, V_DS_HIST, V_DS_CHIST, V_CONTA, V_CCUSTO.

        ENDLOOP.

      ENDLOOP.

*      ENDIF.
      CLEAR: V_VALOR, V_SEQUENCIAL, V_SEQUENCIAL_ITEM.

      IF IT_HEADER[] IS NOT INITIAL.

        CLEAR: W_HEADER, W_DETALHE, W_FILE.

        V_SEQUENCIAL = 000.
        V_SEQUENCIAL_ITEM = 000000000000000.

        LOOP AT IT_HEADER ASSIGNING FIELD-SYMBOL(<W_HEADER>).

          V_SEQUENCIAL = V_SEQUENCIAL + 001.

          DO.
            ASSIGN COMPONENT SY-INDEX OF STRUCTURE <W_HEADER> TO FIELD-SYMBOL(<W_FIELD>).
            IF SY-SUBRC = 0.

              CASE SY-INDEX.
                WHEN 1.
                  W_FILE = <W_FIELD>.
                WHEN 10 OR 11 OR 12 OR 13 OR 19 OR 20 OR 21 OR 22.
                  <W_FIELD> = <W_FIELD> * 100.
                  V_VALOR = CONV #( <W_FIELD> ).
                  W_FILE = |{ W_FILE }{ V_VALOR }|.
                WHEN OTHERS.
                  W_FILE = |{ W_FILE }{ <W_FIELD> }|.
              ENDCASE.

            ELSE.
              EXIT.
            ENDIF.
            REPLACE ALL OCCURRENCES OF '.' IN W_FILE WITH ''.
          ENDDO.

          IF W_FILE IS NOT INITIAL.
            APPEND W_FILE TO IT_FILE[]. CLEAR W_FILE.
          ENDIF.

          CLEAR: V_VALOR.

          LOOP AT IT_DETALHE ASSIGNING FIELD-SYMBOL(<W_DETALHE>) WHERE DD_MOVTO = <W_HEADER>-DD_MOVTO.

            V_SEQUENCIAL_ITEM = V_SEQUENCIAL_ITEM + 000000000000001.

            DO.
              ASSIGN COMPONENT SY-INDEX OF STRUCTURE <W_DETALHE> TO FIELD-SYMBOL(<W_FIELD_DET>).
              IF SY-SUBRC = 0.
                CASE SY-INDEX.
                  WHEN 1.
                    W_FILE = <W_FIELD_DET>.
                  WHEN 7.
                    W_FILE = |{ W_FILE }{ V_SEQUENCIAL_ITEM }|.
                  WHEN 14.
*                    <W_FIELD_DET> = <W_FIELD_DET> * 100.
                    V_VALOR = CONV #( <W_FIELD_DET> ).
                    W_FILE = |{ W_FILE }{ V_VALOR }|.
                  WHEN OTHERS.
                    W_FILE = |{ W_FILE }{ <W_FIELD_DET> }|.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
*              REPLACE ALL OCCURRENCES OF '.' IN W_FILE WITH ''.
            ENDDO.

            IF W_FILE IS NOT INITIAL.
              APPEND W_FILE TO IT_FILE[]. CLEAR W_FILE.
            ENDIF.

          ENDLOOP.

          DATA(V_FILENAME) = |MV04{ <W_HEADER>-AM_MOVTO+2(2) }{ <W_HEADER>-DT_PROC+0(2) }.{ V_SEQUENCIAL }|.

          CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
            EXPORTING
              WINDOW_TITLE              = 'Amaggi SF Gerar arquivo'
              DEFAULT_FILE_NAME         = V_FILENAME
              DEFAULT_EXTENSION         = 'TXT'
              INITIAL_DIRECTORY         = 'C:\'
            CHANGING
              FILENAME                  = VL_FILENAME
              PATH                      = VL_PATH
              FULLPATH                  = VL_FULLPATH
            EXCEPTIONS
              CNTL_ERROR                = 1
              ERROR_NO_GUI              = 2
              NOT_SUPPORTED_BY_GUI      = 3
              INVALID_DEFAULT_FILE_NAME = 4
              OTHERS                    = 5.

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              FILENAME                = VL_FULLPATH
              CONFIRM_OVERWRITE       = 'X'
            TABLES
              DATA_TAB                = IT_FILE[]
            EXCEPTIONS
              FILE_WRITE_ERROR        = 1
              NO_BATCH                = 2
              GUI_REFUSE_FILETRANSFER = 3
              INVALID_TYPE            = 4
              NO_AUTHORITY            = 5
              UNKNOWN_ERROR           = 6
              HEADER_NOT_ALLOWED      = 7
              SEPARATOR_NOT_ALLOWED   = 8
              FILESIZE_NOT_ALLOWED    = 9
              HEADER_TOO_LONG         = 10
              DP_ERROR_CREATE         = 11
              DP_ERROR_SEND           = 12
              DP_ERROR_WRITE          = 13
              UNKNOWN_DP_ERROR        = 14
              ACCESS_DENIED           = 15
              DP_OUT_OF_MEMORY        = 16
              DISK_FULL               = 17
              DP_TIMEOUT              = 18
              FILE_NOT_FOUND          = 19
              DATAPROVIDER_EXCEPTION  = 20
              CONTROL_FLUSH_ERROR     = 21
              OTHERS                  = 22.
          IF SY-SUBRC <> 0.

          ELSE.
            DATA(VL_MSG_ARQ) = | Arquivo '{ VL_FILENAME }' gerado com sucesso! |.
            MESSAGE VL_MSG_ARQ TYPE 'S'.
          ENDIF.

          CLEAR: V_SEQUENCIAL_ITEM, W_FILE, IT_FILE[].

        ENDLOOP.

      ENDIF.

      CLEAR: W_BSIS, W_FAGLFLEXA, W_HEADER, W_DETALHE, W_FILE, V_SEQUENCIAL,
             IT_BSIS[], IT_FAGLFLEXA[], IT_HEADER[], IT_DETALHE[], IT_FILE[].

    ENDIF.


  ENDMETHOD.


  METHOD MONTA_ARQUIVO.

  ENDMETHOD.


  METHOD GET_CONTAS.

    SELECT * FROM ZFIT0153 INTO TABLE IT_CONTAS[] WHERE DT_CADASTRO <> '00000000'.
    SORT IT_CONTAS BY RACCT ASCENDING.

  ENDMETHOD.


  METHOD SET_CONTAS.

    LOOP AT IT_CONTAS[] ASSIGNING FIELD-SYMBOL(<W_CONTAS>) WHERE USNAM IS INITIAL.
      <W_CONTAS>-USNAM = SY-UNAME.
      <W_CONTAS>-DT_CADASTRO = SY-DATUM.
    ENDLOOP.

    SELECT * FROM ZFIT0153 INTO TABLE @DATA(IT_CHECK_CONTAS)
       WHERE DT_CADASTRO <> '00000000'.

    LOOP AT IT_CHECK_CONTAS[] INTO DATA(W_CHECK).

      READ TABLE IT_CONTAS[] WITH KEY RACCT = W_CHECK-RACCT TRANSPORTING NO FIELDS.

      IF SY-SUBRC <> 0.
        DELETE FROM ZFIT0153 WHERE RACCT = W_CHECK-RACCT.
      ENDIF.

    ENDLOOP.

    IF IT_CONTAS[] IS NOT INITIAL.
      MODIFY ZFIT0153 FROM TABLE IT_CONTAS.
    ENDIF.

    MESSAGE 'Contas atualizadas!' TYPE 'S'.

    ME->GET_CONTAS( ).

    ALV_CONTAS->REFRESH_TABLE_DISPLAY( ).

  ENDMETHOD.


  METHOD GRAVA_ARQUIVO.

  ENDMETHOD.


  METHOD GET_PIC_TAB.

    DATA MIME_API TYPE REF TO IF_MR_API.

    MIME_API = CL_MIME_REPOSITORY_API=>GET_API( ).
    MIME_API->GET( EXPORTING
                     I_URL             = MIME_URL
                     I_CHECK_AUTHORITY = ABAP_FALSE
                   IMPORTING
                     E_CONTENT = DATA(PIC_WA)
                   EXCEPTIONS OTHERS = 4 ).
    IF SY-SUBRC = 4.
      RETURN.
    ENDIF.

    CLEAR PIC_TAB.

    DATA(LENGTH) = XSTRLEN( PIC_WA ).

    WHILE LENGTH >= 1022.
      APPEND PIC_WA(1022) TO PIC_TAB.
      SHIFT PIC_WA BY 1022 PLACES LEFT IN BYTE MODE.
      LENGTH = XSTRLEN( PIC_WA ).
    ENDWHILE.
    IF LENGTH > 0.
      APPEND PIC_WA TO PIC_TAB.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'ST01'.
  SET TITLEBAR 'T01'.

  DATA(OBJ_ARQUIVO) = NEW CL_ARQ( ).

  IF ( SPLITTER IS INITIAL ).

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER_PICTURE.

    IF ( PICTURE IS INITIAL ).

      CREATE OBJECT PICTURE
        EXPORTING
          PARENT = CTL_CCCONTAINER_PICTURE
        EXCEPTIONS
          ERROR  = 1.

      CALL METHOD PICTURE->SET_DISPLAY_MODE
        EXPORTING
          DISPLAY_MODE = PICTURE->DISPLAY_MODE_STRETCH
        EXCEPTIONS
          ERROR        = 1.

      PERFORM LOAD_PIC_FROM_DB USING PICTURE.

    ENDIF.

  ENDIF.

ENDMODULE.


SELECTION-SCREEN BEGIN OF SCREEN 0200.
PARAMETERS: P_BUKRS TYPE FAGLFLEXA-RBUKRS OBLIGATORY DEFAULT '0043',
            P_MES   TYPE CHAR2 OBLIGATORY DEFAULT V_MES,
            P_ANO   TYPE FAGLFLEXA-RYEAR OBLIGATORY DEFAULT V_ANO,
            P_USER  TYPE CHAR060.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN END OF SCREEN 0200.


FORM LOAD_PIC_FROM_DB  USING GUI_PICTURE TYPE REF TO CL_GUI_PICTURE.

  TYPES PIC_LINE(1022) TYPE X.
  DATA  PIC_TAB TYPE TABLE OF PIC_LINE.

  DATA(URL) = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'.

  CL_ARQ=>GET_PIC_TAB(
     EXPORTING MIME_URL = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'
     IMPORTING PIC_TAB  = PIC_TAB ).

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE    = 'image'
      SUBTYPE = 'GIF'
    TABLES
      DATA    = PIC_TAB
    CHANGING
      URL     = URL
    EXCEPTIONS
      OTHERS  = 1.

  CALL METHOD GUI_PICTURE->LOAD_PICTURE_FROM_URL
    EXPORTING
      URL = URL.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.

    WHEN 'BACK'.
      LEAVE TO SCREEN '0'.

    WHEN 'GERAR'.

      V_MES = |{ SY-DATUM+4(2) }|.
      V_ANO = |{ SY-DATUM+0(4) }|.
      CALL SCREEN '0200' STARTING AT 2 2 ENDING AT 100 5.

    WHEN 'CONTAS'.

      OBJ_ARQUIVO->GET_CONTAS( ).

      CALL SCREEN '0300' STARTING AT 2 2 ENDING AT 60 20.

  ENDCASE.

  IF SY-UCOMM IS INITIAL AND P_USER IS NOT INITIAL.

    OBJ_ARQUIVO->GET_DADOS(
      EXPORTING
        I_MONAT = P_MES
        I_YEAR  = P_ANO
        I_BUKRS = P_BUKRS
        I_USUARIO = P_USER
    ).

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.

  TYPES:  T_FIELDCAT          TYPE TABLE OF LVC_S_FCAT WITH DEFAULT KEY.

  SET PF-STATUS 'ST03'.
  SET TITLEBAR 'T03'.

  IF ALV_CONTAS IS INITIAL.

    CREATE OBJECT CONTAINER_CONTAS EXPORTING CONTAINER_NAME = 'CONTAINER_CONTAS'.
    CREATE OBJECT ALV_CONTAS EXPORTING I_PARENT = CONTAINER_CONTAS.

    DATA(TL_FIELDCAT) = VALUE T_FIELDCAT(
    ( FIELDNAME = 'RACCT'       COLTEXT = 'Conta'         EDIT = 'X'  COL_OPT = ''   EMPHASIZE = '' )
    ( FIELDNAME = 'CONTA_BANCO' COLTEXT = 'Conta Banco'   REF_FIELD = 'CONTA_BANCO'  REF_TABLE = 'ZFIT0153' EDIT = 'X'  COL_OPT = ''  EMPHASIZE = '' )
    ( FIELDNAME = 'USNAM'       COLTEXT = 'Criado por'    EDIT = ''   COL_OPT = 'X'  EMPHASIZE = 'C200' )
    ( FIELDNAME = 'DT_CADASTRO' COLTEXT = 'Data Cadastro' EDIT = ''   COL_OPT = 'X'  EMPHASIZE = 'C200' ) ).

    DATA(GS_LAYOUT) = VALUE LVC_S_LAYO( SEL_MODE  = 'A' STYLEFNAME = 'CELLSTYLES' ).

    "SET HANDLER: ME->HANDLE_DATA_CHANGED FOR ALV_CONTAS.

    CALL METHOD ALV_CONTAS->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = GS_LAYOUT
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = IT_CONTAS[]
        IT_FIELDCATALOG               = TL_FIELDCAT[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    CALL METHOD ALV_CONTAS->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD ALV_CONTAS->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
      EXCEPTIONS
        ERROR      = 1
        OTHERS     = 2.

  ELSE.

    ALV_CONTAS->REFRESH_TABLE_DISPLAY( ).

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  CASE SY-UCOMM.
    WHEN 'SAVE'.

      OBJ_ARQUIVO->SET_CONTAS( ).

    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
