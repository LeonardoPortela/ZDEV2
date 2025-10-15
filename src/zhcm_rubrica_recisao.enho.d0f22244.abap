"Name: \TY:CL_HRPAYBR_TERM\ME:PROCESS_EMPLOYEE\SE:END\EI
ENHANCEMENT 0 ZHCM_RUBRICA_RECISAO.
*BUSCA NA PC_PAYRESULT A RUBRICA /335 - 01.06.2018
 IF ( SY-TCODE EQ 'PC00_M37_HBRRTER3' ).


   DATA: C_CLUSTERBR      TYPE PCL2-RELID  VALUE 'BR',
         VL_PERNR         TYPE PERNR_D,
         IT_RGDIR         TYPE TABLE OF PC261,
         P_RESULT         TYPE PAYBR_RESULT.
   FIELD-SYMBOLS: <WL_PROVISION> LIKE LINE OF LT_PROVISION_DATA[].

   LOOP AT LT_PROVISION_DATA[] ASSIGNING <WL_PROVISION> WHERE SUMLG EQ '0630'.

     MOVE LS_EMPLOYEE_DATA-EMPLOYEE_ID_DATA-PERNR TO VL_PERNR.


     CALL FUNCTION 'CU_READ_RGDIR_NEW'
      EXPORTING
        PERSNR                = VL_PERNR
      TABLES
        IN_RGDIR              = IT_RGDIR
      EXCEPTIONS
        NO_RECORD_FOUND       = 1
        IMPORT_MISMATCH_ERROR = 2
        NO_READ_AUTHORITY     = 3
        INCONSISTENT_DATA     = 4
        OTHERS                = 5.

     SORT IT_RGDIR BY SEQNR DESCENDING.
     READ TABLE IT_RGDIR INTO DATA(WL_RGDIR) INDEX 1.

     CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
          EXPORTING
            CLUSTERID                    = C_CLUSTERBR
            EMPLOYEENUMBER               = VL_PERNR " Personnel Number
            SEQUENCENUMBER               = WL_RGDIR-SEQNR "P_SEQNR  " Sequence Number
          CHANGING
            PAYROLL_RESULT               = P_RESULT    " Complex Structure For Payroll Result
          EXCEPTIONS
            ILLEGAL_ISOCODE_OR_CLUSTERID = 1
            ERROR_GENERATING_IMPORT      = 2
            IMPORT_MISMATCH_ERROR        = 3
            SUBPOOL_DIR_FULL             = 4
            NO_READ_AUTHORITY            = 5
            NO_RECORD_FOUND              = 6
            VERSIONS_DO_NOT_MATCH        = 7
            OTHERS                       = 8.

     READ TABLE P_RESULT-INTER-RT INTO DATA(WL_RESULT) WITH KEY LGART = '/335'.

     MOVE WL_RESULT-ANZHL TO <WL_PROVISION>-NUMBER.

   ENDLOOP.

   CLEAR: ls_employee_data-provisions[].

   me->set_wages_to_term_output_table(
      EXPORTING
        it_wages      = lt_provision_data[]
      IMPORTING
        et_term_wages = ls_employee_data-provisions[] ).

* ---------------------------------------------------------------------
* Decide which attachment will be printed
* ---------------------------------------------------------------------

  TRY .
    ls_employee_data-print_term_discharge = me->get_print_option( ).

  CATCH cx_hrpaybr_exception INTO lx_exception.

    me->append_pernr_exception(
      EXPORTING io_exception = lx_exception
                iv_msgty     = 'W' ).

    ls_employee_data-print_term_discharge =
      if_hrpaybr_term_constants=>mc_print_term_discharge.

  ENDTRY.

   data(v_index) = lines( MT_EMPLOYEE_DATA[] ).
   MODIFY MT_EMPLOYEE_DATA[] FROM ls_employee_data INDEX v_index.

 ENDIF.


ENDENHANCEMENT.
