"Name: \PR:HBRCCED0\FO:PRINT_FORM_VIA_FRAMEWORK\SE:BEGIN\EI
ENHANCEMENT 0 Z_INFORME_IR.

*CONSTANTS C_X TYPE C VALUE 'X'.
*
DATA: FM_NAME         TYPE RS38L_FNAM,
      FPNAME          TYPE FPNAME,
      LS_CONTROL      TYPE SSFCTRLOP,
      VL_NAME         TYPE RS38L_FNAM,
      LS_DATA2        TYPE pbr_form_cced,
      LS_PDF_STRING_X TYPE XSTRING,
      ROOT            TYPE REF TO ZCL_MEMORY_VARIAVEIS,
      OREF            TYPE REF TO ZCL_MEMORY_VARIAVEIS,
      JOB_OUTPUT_INFO TYPE SSFCRESCL,
      E_OTF01         TYPE TT_ITCOO.

DATA: VL_FORM TYPE TDSFNAME,
      v_mask_locl(60) TYPE c,
      v_cont  type i,
      v_cpf(11),
      c_cont,
      p_input(60),
      t_dir_local     TYPE TABLE OF sdokpath,
      t_dir_loc_f     TYPE TABLE OF sdokpath.
*
*"PDF
DATA: I_OTF           TYPE ITCOO OCCURS 0 WITH HEADER LINE,
      I_TLINE         TYPE TABLE OF TLINE WITH HEADER LINE,
      V_BIN_FILESIZE  TYPE I,
      I_RECORD        LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
      WA_BUFFER       TYPE STRING,
      LV_NOME_ARQUIVO TYPE STRING,
      LV_NOME         TYPE STRING.
*
DATA: LS_OPTIONS2      TYPE SSFCOMPOP,
      v_bol            TYPE abap_bool.

FPNAME = 'HRPAYBR_CCED0'.
CLEAR: FM_NAME.

DESCRIBE TABLE hcc LINES DATA(LC_QTD).

IF LC_QTD EQ 1.

  READ TABLE hcc into data(wa_hcc) INDEX 1.
  CONCATENATE 'PDF_Informe_Rendimentos' wa_hcc-PERNR INTO DATA(NM_INSTANCE).

  TRY.
      DATA(HANDLE) = ZCL_MEMORY_VARIAVEIS_AREA=>ATTACH_FOR_READ( INST_NAME = CONV #( NM_INSTANCE ) ).
      DATA(CK_INSTANCE) = ABAP_TRUE.
      HANDLE->DETACH( ).
    CATCH CX_SHM_ATTACH_ERROR.
      CK_INSTANCE = ABAP_FALSE.
  ENDTRY.

  if CK_INSTANCE eq abap_true.

    LS_CONTROL-PREVIEW   = SPACE.
    LS_CONTROL-DEVICE    = 'PRINTER'.
    LS_CONTROL-GETOTF    = ABAP_TRUE.
    LS_CONTROL-NO_DIALOG = ABAP_TRUE.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = FPNAME
      IMPORTING
        FM_NAME            = FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.

    IF SY-SUBRC is NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR ls_data2.

    LOOP AT dcc WHERE pernr EQ wa_hcc-pernr.

      CLEAR: ls_data2.
      PERFORM move_hcc USING wa_hcc CHANGING ls_data2.
      PERFORM move_dcc USING dcc CHANGING ls_data2.

      DATA: lc_char_14 TYPE c LENGTH 14.

      lc_char_14 = ls_data2-cgc.

      if lc_char_14(8) eq '23771214'.
        CONTINUE.
      endif.

      CALL FUNCTION FM_NAME
        EXPORTING
          USER_SETTINGS      = ' '
          CONTROL_PARAMETERS = LS_CONTROL
          IS_PBR_FORM_CCED   = LS_DATA2
        IMPORTING
          JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.

      IF SY-SUBRC is NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION 'ZSMARTFORMS_MERGE_OTF'
        EXPORTING
          OTF_01           = E_OTF01
          OTF_02           = JOB_OUTPUT_INFO-OTFDATA[]
       IMPORTING
         OTF_RESULT       = E_OTF01.

    ENDLOOP.

    CALL FUNCTION 'ZSMARTFORMS_OTF_XSTRING'
      EXPORTING
        OTF            = E_OTF01
     IMPORTING
       LS_PDF_STRING_X = LS_PDF_STRING_X.

    HANDLE = ZCL_MEMORY_VARIAVEIS_AREA=>ATTACH_FOR_WRITE( INST_NAME = CONV #( NM_INSTANCE ) ).
    CREATE OBJECT ROOT AREA HANDLE HANDLE.
    HANDLE->SET_ROOT( ROOT ).
    CREATE OBJECT ROOT AREA HANDLE HANDLE TYPE ZCL_MEMORY_VARIAVEIS.
    OREF ?= ROOT.
    OREF->SET_TEXTO_XSTRING( I_XSTRING = LS_PDF_STRING_X
       )->SET_TEXTO_OTF( I_OTF = E_OTF01
       ).
    CLEAR OREF.
    HANDLE->SET_ROOT( ROOT ).
    HANDLE->DETACH_COMMIT( ).
    LEAVE PROGRAM.

  ENDIF.
ENDIF.

LV_NOME_ARQUIVO = filename.

V_CONT = STRLEN( LV_NOME_ARQUIVO ).
V_CONT = V_CONT - 1.
IF V_CONT LE 0 OR LV_NOME_ARQUIVO+V_CONT(1) NE '\'.
   MESSAGE S000(Z01) WITH 'Nome Diretorio inválido!'.
   exit.
ENDIF.

CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
        directory = LV_NOME_ARQUIVO
        RECEIVING
        result = v_bol
        EXCEPTIONS
        cntl_error = 1
        error_no_gui = 2
        wrong_parameter = 3
        not_supported_by_gui = 4
        OTHERS = 5.

        IF v_bol IS INITIAL.
          MESSAGE S000(Z01) WITH 'Diretorio não existe'.
          exit.
        ENDIF.
*  Impresora
LS_CONTROL-NO_DIALOG = 'X'.
LS_OPTIONS2-TDDEST   = 'LOCL'.
LS_OPTIONS2-TDIMMED  = 'X'.
LS_OPTIONS2-TDNEWID  = 'X'.
LS_OPTIONS2-TDNOARCH = 'X'.
LS_OPTIONS2-TDNOPREV = 'X'.

LS_CONTROL-PREVIEW = SPACE.
LS_CONTROL-DEVICE  = 'PRINTER'.
LS_CONTROL-GETOTF  = 'X'.

CLEAR:JOB_OUTPUT_INFO.

* Get generated function module's name, using logical SmartForms name
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = FPNAME
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID
          TYPE SY-MSGTY
        NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*VL_NAME = '/1BCDWB/SF00000151'.
VL_NAME = FM_NAME.
p_input = filename.

LOOP AT hcc.

  CLEAR ls_data2.
  ADD 1 TO l_nproc.
  PERFORM progress_ind(hbrigui0)
  USING l_nproc total-proc. " Show Progress Ind.

  PERFORM move_hcc USING hcc
                CHANGING ls_data2.

  LOOP AT dcc WHERE pernr EQ hcc-pernr.

    PERFORM move_dcc USING dcc
                  CHANGING ls_data2.

    REFRESH: I_RECORD,I_TLINE, I_OTF.
    CALL FUNCTION VL_NAME
      EXPORTING
        USER_SETTINGS      = ' '
        CONTROL_PARAMETERS = LS_CONTROL
        OUTPUT_OPTIONS     = LS_OPTIONS2
        IS_PBR_FORM_CCED   = LS_DATA2
      IMPORTING
        JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
*        TABLES
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

    I_OTF[] = JOB_OUTPUT_INFO-OTFDATA[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
        MAX_LINEWIDTH         = 132
      IMPORTING
        BIN_FILESIZE          = V_BIN_FILESIZE
      TABLES
        OTF                   = I_OTF
        LINES                 = I_TLINE
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        OTHERS                = 4.

    LOOP AT I_TLINE.
      TRANSLATE I_TLINE USING '~'.
      CONCATENATE WA_BUFFER I_TLINE INTO WA_BUFFER.
    ENDLOOP.
    TRANSLATE WA_BUFFER USING '~'.
    DO.
      I_RECORD = WA_BUFFER.
      APPEND I_RECORD.
      SHIFT WA_BUFFER LEFT BY 255 PLACES.
      IF WA_BUFFER IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    "
    IF LS_DATA2-CPF is INITIAL.
      CONTINUE.
    ENDIF.
    LV_NOME = LS_DATA2-CPF.
    REPLACE ALL OCCURRENCES OF '.' IN LV_NOME WITH SPACE.
    REPLACE ALL OCCURRENCES OF '-' IN LV_NOME WITH SPACE.
    CONDENSE LV_NOME.
    v_cpf = LV_NOME.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = v_cpf
        IMPORTING
          OUTPUT = v_cpf .

    LV_NOME = v_cpf.
    "
    CONCATENATE filename v_cpf   INTO LV_NOME_ARQUIVO.
    CONCATENATE LV_NOME '*.PDF' into v_mask_locl.

    refresh: t_dir_loc_f, t_dir_local.
    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        directory        = p_input
        filter           = v_mask_locl
      TABLES
        file_table       = t_dir_loc_f
        dir_table        = t_dir_local
   EXCEPTIONS
     cntl_error       = 1
     OTHERS           = 2.

    v_cont = lines( t_dir_loc_f ).
    if v_cont = 0.
        CONCATENATE LV_NOME_ARQUIVO '.PDF'  INTO LV_NOME_ARQUIVO.
    else.
      c_cont = v_cont.
      CONCATENATE LV_NOME_ARQUIVO '_' c_cont '.PDF'  INTO LV_NOME_ARQUIVO.
    endif.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME = LV_NOME_ARQUIVO
        FILETYPE = 'BIN'
      TABLES
        DATA_TAB = I_RECORD[].

  ENDLOOP.

ENDLOOP.

ENDENHANCEMENT.
