*&---------------------------------------------------------------------*
*& Report  ZMMCESTOQUE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  ZMMCESTOQUE no standard page heading message-id SD.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
types: begin of TYPE_LINE,
         LINE type CHAR600,
       end   of TYPE_LINE.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
data: T_LINE type table of TYPE_LINE.

data: VL_MAT_DOC         type BAPI2017_GM_HEAD_RET-MAT_DOC,
      VL_MATDOCUMENTYEAR type BAPI2017_GM_HEAD_RET-DOC_YEAR.


data:
* Tabela de mapeamento de tela da transação do BI
  T_BDC              type table of BDCDATA,
  V_MODE             type C value 'N',
  WA_GOODSMVT_HEADER type BAPI2017_GM_HEAD_01,
  WA_GOODSMVT_ITEM   type BAPI2017_GM_ITEM_CREATE,
  WA_CODE            type BAPI2017_GM_CODE,
  T_GOODSMVT_ITEM    type table of BAPI2017_GM_ITEM_CREATE,
  T_RETURN           type table of BAPIRET2.

data: ITAB type BDCMSGCOLL occurs 0 with header line.
*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
constants: C_FILETXT type CHAR26 value 'Files CSV (*.CSV)|*.CSV|',
           C_INICIAL type CHAR3  value 'C:\',
           C_X       type CHAR1  value 'X'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
selection-screen begin of block A1 with frame title text-001.
  selection-screen begin of block A2 with frame.
    parameters
      P_FILE   type SDBA_A_NAM obligatory.
  selection-screen end   of block A2.
selection-screen end   of block A1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
at selection-screen on value-request for P_FILE.

* Ajuda Pesquiza Campo Filename
  perform Z_AJUDA_FILENAME.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
start-of-selection.

* Upload do Arquivo
  perform: Z_UPLOAD  ,
* Processa Arquivo
           Z_PROCESSA.

*&---------------------------------------------------------------------*
*&      Form  Z_AJUDA_FILENAME                                         *
*&---------------------------------------------------------------------*
*                      Ajuda Pesquiza Campo Filename                   *
*----------------------------------------------------------------------*
form Z_AJUDA_FILENAME.

  data: VL_TITLE   type STRING,
        VL_FILTER  type STRING,
        VL_INITIAL type STRING,
        VL_RC      type I,
        TL_FILE    type FILETABLE.

  VL_TITLE   = text-002.
  VL_FILTER  = C_FILETXT.
  VL_INITIAL = C_INICIAL.

  call method CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    exporting
      WINDOW_TITLE            = VL_TITLE
      FILE_FILTER             = VL_FILTER
      INITIAL_DIRECTORY       = VL_INITIAL
    changing
      FILE_TABLE              = TL_FILE
      RC                      = VL_RC
    exceptions
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      others                  = 5.

  if not SY-SUBRC is initial.
    message id SY-MSGID type SY-MSGTY number SY-MSGNO
               with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    exit.
  endif.

  read table TL_FILE into P_FILE index 1.

endform.                    " Z_AJUDA_FILENAME


*&---------------------------------------------------------------------*
*&      Form  Z_UPLOAD                                                 *
*&---------------------------------------------------------------------*
*                              Upload Arquivo                          *
*----------------------------------------------------------------------*
form Z_UPLOAD.

  data VL_FILE type STRING.

  refresh T_LINE.

  VL_FILE = P_FILE.

  call method CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    exporting
      FILENAME                = VL_FILE
      FILETYPE                = 'ASC'
    changing
      DATA_TAB                = T_LINE
    exceptions
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      NOT_SUPPORTED_BY_GUI    = 17
      ERROR_NO_GUI            = 18
      others                  = 19.

  if not SY-SUBRC is initial.
    message id SY-MSGID type SY-MSGTY number SY-MSGNO
               with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    exit.
  endif.

  delete T_LINE index 1.

  if T_LINE[] is initial.
    message I836 with text-003.
    leave list-processing.
  endif.

endform.                    " Z_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA                                               *
*&---------------------------------------------------------------------*
*                             Processa Arquivo                         *
*----------------------------------------------------------------------*
form Z_PROCESSA.

  data: SL_LINE type TYPE_LINE,
        begin of WA_T0002,
          BLDAT	     type C length 10,
          BUDAT      type C length 10,
          BWARTWA    type BWARTWA,
          WERKS      type WERKS,
          LGORT      type LGORT_D,
          MATNR      type MATNR,
          ERFMG      type STRING,
          CHARG      type C length 10,
          GSBER      type GSBER,
          GL_ACCOUNT type SAKNR,
          AMOUNT_LC  type STRING,
          COSTCENTER type C length 10,
          ORDERID    type C length 12,
        end of WA_T0002,
        TL_T0002 like standard table of WA_T0002.

  loop at T_LINE into SL_LINE.
    clear WA_T0002.
    split SL_LINE-LINE at ';' into WA_T0002-BLDAT
                                   WA_T0002-BUDAT
                                   WA_T0002-BWARTWA
                                   WA_T0002-WERKS
                                   WA_T0002-LGORT
                                   WA_T0002-MATNR
                                   WA_T0002-ERFMG
                                   WA_T0002-CHARG
                                   WA_T0002-GSBER
                                   WA_T0002-GL_ACCOUNT
                                   WA_T0002-AMOUNT_LC
                                   WA_T0002-COSTCENTER
                                   WA_T0002-ORDERID.

    concatenate WA_T0002-BLDAT+6(4) WA_T0002-BLDAT+3(2) WA_T0002-BLDAT(2) into WA_T0002-BLDAT.
    concatenate WA_T0002-BUDAT+6(4) WA_T0002-BUDAT+3(2) WA_T0002-BUDAT(2) into WA_T0002-BUDAT.

    replace all occurrences of regex '[^0-9,]' in WA_T0002-ERFMG with ''.
    replace all occurrences of regex ',' in WA_T0002-ERFMG with '.'.
    replace all occurrences of regex ',' in WA_T0002-AMOUNT_LC with '.'.
    if not WA_T0002-BLDAT is initial.
      append WA_T0002 to TL_T0002.
    endif.
    clear SL_LINE.
  endloop.

  loop at TL_T0002 into WA_T0002.

    clear: T_GOODSMVT_ITEM[], WA_GOODSMVT_HEADER, WA_CODE, VL_MAT_DOC, VL_MATDOCUMENTYEAR, T_RETURN.

    WA_GOODSMVT_HEADER-PSTNG_DATE = WA_T0002-BUDAT.
    WA_GOODSMVT_HEADER-DOC_DATE	  = WA_T0002-BLDAT.
    if ( WA_T0002-COSTCENTER is initial ) and ( WA_T0002-ORDERID is initial  ).
      WA_CODE-GM_CODE = '05'.
    else.
      WA_CODE-GM_CODE = '03'.
    endif.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        INPUT        = WA_T0002-MATNR
      importing
        OUTPUT       = WA_T0002-MATNR
      exceptions
        LENGTH_ERROR = 1
        others       = 2.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_T0002-COSTCENTER
      importing
        OUTPUT = WA_T0002-COSTCENTER.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_T0002-ORDERID
      importing
        OUTPUT = WA_T0002-ORDERID.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_T0002-GL_ACCOUNT
      importing
        OUTPUT = WA_T0002-GL_ACCOUNT.

*--> 15.06.2023 - Migration S4 – MIGNOW - Start
    "    wa_goodsmvt_item-material     = wa_t0002-matnr.
    data(V_LEN) = STRLEN( WA_T0002-MATNR ).
    if V_LEN > 18.
      WA_GOODSMVT_ITEM-MATERIAL_LONG = WA_T0002-MATNR .
    else.
      WA_GOODSMVT_ITEM-MATERIAL = WA_T0002-MATNR .
    endif.
*<-- 15.06.2023 - Migration S4 – MIGNOW – End
    WA_GOODSMVT_ITEM-MOVE_PLANT   = WA_T0002-WERKS.
    WA_GOODSMVT_ITEM-PLANT        = WA_T0002-WERKS.
    WA_GOODSMVT_ITEM-MOVE_STLOC   = WA_T0002-LGORT.
    WA_GOODSMVT_ITEM-STGE_LOC     = WA_T0002-LGORT.
    WA_GOODSMVT_ITEM-BATCH        = WA_T0002-CHARG.
    WA_GOODSMVT_ITEM-MOVE_BATCH   = WA_T0002-CHARG.
    WA_GOODSMVT_ITEM-MOVE_TYPE    = WA_T0002-BWARTWA.
    WA_GOODSMVT_ITEM-ENTRY_QNT   	= WA_T0002-ERFMG.
    WA_GOODSMVT_ITEM-GL_ACCOUNT   = WA_T0002-GL_ACCOUNT.
    WA_GOODSMVT_ITEM-AMOUNT_LC   	= WA_T0002-AMOUNT_LC.
    WA_GOODSMVT_ITEM-COSTCENTER   = WA_T0002-COSTCENTER.
    WA_GOODSMVT_ITEM-ORDERID      = WA_T0002-ORDERID.
    WA_GOODSMVT_ITEM-ITEM_TEXT    = 'Carga estoque matéria prima'.
    append WA_GOODSMVT_ITEM to T_GOODSMVT_ITEM.

    call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      exporting
        GOODSMVT_HEADER  = WA_GOODSMVT_HEADER
        GOODSMVT_CODE    = WA_CODE
      importing
        MATERIALDOCUMENT = VL_MAT_DOC
        MATDOCUMENTYEAR  = VL_MATDOCUMENTYEAR
      tables
        GOODSMVT_ITEM    = T_GOODSMVT_ITEM
        RETURN           = T_RETURN.

    if SY-SUBRC eq 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          WAIT = C_X.
    endif.

  endloop.

endform.                    " Z_PROCESSA

*----------------------------------------------------------------------*
***INCLUDE LZSDG002F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_INSERE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0012   text
*      -->P_0013   text
*      -->P_0014   text
*----------------------------------------------------------------------*
form Z_INSERE_BDC  using P_DYNBEGIN
                         P_FIELD
                         P_VALUE.

  data: WA_BDC type BDCDATA.

  clear WA_BDC.

  if P_DYNBEGIN eq 'X'.
    WA_BDC-DYNBEGIN = 'X'.
    WA_BDC-PROGRAM  = P_FIELD.
    WA_BDC-DYNPRO   = P_VALUE.
  else.
    WA_BDC-FNAM = P_FIELD.
    WA_BDC-FVAL = P_VALUE.
  endif.

  append WA_BDC to T_BDC.

endform.                    " Z_INSERE_BDC



*    CLEAR: t_bdc[], t_bdc, itab[], itab.
*    PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'      '0400',
*                                ' ' 'BDC_CURSOR'    'RM07M-LGORT',
*                                ' ' 'BDC_OKCODE'    '/00',
*                                ' ' 'MKPF-BLDAT'    wa_t0002-bldat,
*                                ' ' 'MKPF-BUDAT'    wa_t0002-budat,
*                                ' ' 'RM07M-BWARTWA'  wa_t0002-bwartwa,
*                                ' ' 'RM07M-WERKS'    wa_t0002-werks,
*                                ' ' 'RM07M-LGORT'    wa_t0002-lgort,
*                                ' ' 'RM07M-WVERS2'  'X'.
*
*    PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'        '0421',
*                                ' ' 'BDC_CURSOR'      'MSEG-CHARG(01)',
*                                ' ' 'BDC_OKCODE'      '/00',
*                                ' ' 'MSEG-MATNR(01)'  wa_t0002-matnr,
*                                ' ' 'MSEG-ERFMG(01)'  wa_t0002-erfmg,
*                                ' ' 'MSEG-CHARG(01)'  wa_t0002-charg,
*                                ' ' 'BDC_SUBSCR'      'SAPMM07M',
*                                ' ' 'BDC_SUBSCR'      'SAPLKACB',
*                                ' ' 'COBL-GSBER'      wa_t0002-gsber.
*
*    PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'   '0421',
*                                ' ' 'BDC_CURSOR' 'MSEG-ERFMG(01)',
*                                ' ' 'BDC_OKCODE' '=BU',
*                                ' ' 'BDC_SUBSCR' 'SAPMM07M',
*                                ' ' 'BDC_SUBSCR' 'SAPLKACB'.
*
*    CALL TRANSACTION 'MB1C' USING t_bdc MODE v_mode MESSAGES INTO itab.
