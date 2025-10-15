class ZCL_NFSE_INBOUND_V1 definition
  public
  final
  create public .

public section.

  interfaces /TCSR/C_CONSTANTS .

  methods CONSTRUCTOR
    importing
      !I_GUID type /TCSR/E_GUID_HEADER .
  methods SET_NEW_GUID
    importing
      !IV_GUID_HEADER type /TCSR/E_GUID_HEADER
      !IV_ACTION type /TCSR/E_ACTION
      !IV_CALL_STEP type /TCSR/E_STEP
      !IV_GUID_CANCEL type /TCSR/E_GUID_HEADER
      !IV_TOLERANCE type /TCSR/E_TOLERANCE
      !IV_TOLERANCE_TAX type /TCSR/E_TOLERANCE_TAX
      !IV_NFTYPE type J_1BNFTYPE
      !IV_CANCREASON type /TCSR/E_CANCR
      !IV_PROCTYP type /TCSR/E_PROCTYP
      !IV_NOT_CREATE type CHAR1
      !IT_PO_LIST_UNASS type /TCSR/Y_PO_LIST
      !IT_PO_LIST_ASSIG type /TCSR/Y_PO_LIST
      !IT_CANCR_TEXT type TSFOTABL
      !IV_APPROVED_DIFF type /TCSR/E_APPROVE_DIFF optional
      !IV_INSS_BASE type /TCSR/E_INSS_BASE optional
      !IV_IR_BASE type /TCSR/E_IR_BASE optional
      !IV_ZLSPR type DZLSPR optional
      !IV_MANUAL type CHAR1 optional
    raising
      /TCSR/CX_EXCEPTION .
  methods PROCFLOW_EXECUTION
    raising
      /TCSR/CX_EXCEPTION .
  methods EXECUTE_ML81N_CREATE
    importing
      !IT_SERV_LINE type /TCSR/Y_PO_LIST
    exporting
      !EV_ENTRYSHEET type BAPIESSR-SHEET_NO
      !EV_ACCEPTANCE type KZABN
      !EV_PACKNO type PACKNO .
  methods EXECUTE_MIGO_CREATE
    importing
      !IT_PO_LIST type /TCSR/Y_PO_LIST
    exporting
      !EV_MBLNR type MBLNR
      !EV_MJAHR type MJAHR .
  methods GET_EXECUTION_MESSAGES
    returning
      value(RT_MESSAGES) type BAPIRET2_T .
  methods EXECUTE_MIRO_CREATE .
protected section.
private section.

  data MT_PO type /TCSR/Y_PO .
  data MW_PO type /TCSR/T_PO .
  data MV_GUID_HEADER type /TCSR/E_GUID_HEADER .
  data MV_ACTION type /TCSR/E_ACTION .
  data MV_GUID_CANCEL type /TCSR/E_GUID_HEADER .
  data MV_EXEC_STEP type /TCSR/E_STEP .
  data MV_TOLERANCE type /TCSR/E_TOLERANCE .
  data MV_TOLERANCE_TAX type /TCSR/E_TOLERANCE_TAX .
  data MV_NFTYPE type J_1BNFTYPE .
  data MV_CANCREASON type /TCSR/E_CANCR .
  data MT_PO_LIST_UNASS type /TCSR/Y_PO_LIST .
  data MT_PO_LIST_ASSIG type /TCSR/Y_PO_LIST .
  data MW_PO_LIST_ASSIG type /TCSR/S_PO_LIST .
  data MT_CANCR_TEXT type TSFOTABL .
  data MW_XML_RPSX type /TCSR/S_RPSX .
  data MW_ACT type /TCSR/T_ACT .
  data MW_HD type /TCSR/T_HD .
  data MW_NFSTX type /TCSR/T_NFSTX .
  data MT_STA type /TCSR/Y_STA .
  data MO_UTIL_XML type ref to /TCSR/C_UTIL_XML .
  data MW_STA type /TCSR/T_STA .
  data MT_STEP type /TCSR/Y_STEP .
  data MV_STEP_START type /TCSR/E_STEP .
  data MV_TABIX_STA type SY-TABIX .
  data MT_WSFILE_DEL type /TCSR/Y_WSFILE .
  data MW_ACT_UPD type /TCSR/T_ACT .
  data MW_NFSTX_UPD type /TCSR/T_NFSTX .
  data MT_STA_UPD type /TCSR/Y_STA .
  data MT_STA_DEL type /TCSR/Y_STA .
  data MT_HIST_UPD type /TCSR/Y_HIST .
  data MT_PO_UPD type /TCSR/Y_PO .
  data MT_PO_DEL type /TCSR/Y_PO .
  data MV_LAST_HISTCOUNT type /TCSR/T_HIST-HISTCOUNT .
  data MV_DUMMY type CHAR255 .
  data MO_BAL_LOG type ref to /TCSR/C_BAL_LOG .
  data MO_PREREQ type ref to /TCSR/C_PREREQ .
  data MV_PROCTYP_MANUAL type /TCSR/E_PROCTYP .
  data MT_REF type /TCSR/Y_REF .
  data MW_REF type /TCSR/T_REF .
  data MV_JOB_NAME type BTCJOB .
  data MV_JOB_USER type SY-UNAME .
  data MT_BAPIRETURN type BAPIRET2_T .
  data MT_PROC_DOC type /TCSR/Y_PROC_DOC .
  data MT_PROC_DOC_UPD type /TCSR/Y_PROC_DOC .
  data MT_PROC_DOC_DEL type /TCSR/Y_PROC_DOC .
  data MV_NOT_CREATE type CHAR1 .
  data MT_PROC_GRP type /TCSR/Y_PROC_GRP .
  data MT_PROC_DET type /TCSR/Y_PROC_DET .
  data MV_INSS_BASE type /TCSR/E_INSS_BASE .
  data MV_IR_BASE type /TCSR/E_IR_BASE .
  data MV_APPROVED_DIFF type /TCSR/E_APPROVE_DIFF .
  data MW_APPROVAL type /TCSR/T_APPROVAL .
  data MV_ZLSPR type DZLSPR .
  data MV_MANUAL type CHAR1 .
  data MT_MESS type BAPIRET2_T .

  methods MESSAGE_FORMAT .
  methods CHECK_CALL_UNDO_STEP
    importing
      !IV_CALL_STEP type /TCSR/E_STEP .
  methods GET_UNDO_STEP
    importing
      !IV_EXEC_STEP type /TCSR/E_STEP
    exporting
      !EV_UNDO_STEP type /TCSR/E_STEP
      !EW_PREV_STA type /TCSR/T_STA
      !EW_NEXT_STA type /TCSR/T_STA .
  methods GET_PREVIOUS_STEP
    importing
      !IV_EXEC_STEP type /TCSR/E_STEP
    exporting
      !EW_CURR_STA type /TCSR/T_STA
      !EW_PREV_STA type /TCSR/T_STA .
  methods ENQUEUE_GUID
    raising
      /TCSR/CX_EXCEPTION .
  methods DEQUEUE_GUID .
  methods CLEAR_ATTRIBUTES .
  methods LOAD_NFSE
    raising
      /TCSR/CX_EXCEPTION .
  methods PROCSTEP_PORTAL .
  methods PROCSTEP_DOWNLOAD .
  methods PROCSTEP_FILENAME .
  methods PROCSTEP_PREREQ .
  methods PROCSTEP_DOCREF .
  methods PROCSTEP_PROCDET .
  methods PROCSTEP_MIGO .
  methods PROCSTEP_ML81N .
  methods PROCSTEP_POASSIGN .
  methods PROCSTEP_VALIDATE .
  methods PROCSTEP_MIRO .
  methods PROCSTEP_CLOSE .
  methods PROCSTEP_CANCEL .
  methods EXECUTE_PORTAL_CONTINUE .
  methods EXECUTE_JOB_PROCSTEP .
  methods EXECUTE_DOWNLOAD_CONTINUE .
  methods EXECUTE_PREREQ
    importing
      !IV_NO_SUCCESS_MSG type ABAP_BOOL optional
    returning
      value(RV_SUBRC) type SUBRC .
  methods EXECUTE_DOCREF .
  methods EXECUTE_PROCDET1 .
  methods EXECUTE_SAVE_PROCDET .
  methods EXECUTE_PROCDET .
  methods EXECUTE_CHECK_DOCREF .
  methods EXECUTE_SAVE_PROCESS .
  methods EXECUTE_MIGO_SAVE
    importing
      !IV_NO_SUCCESS_MSG type ABAP_BOOL optional .
  methods EXECUTE_MIGO_REVERSE .
  methods EXECUTE_MIGO_CONTINUE .
  methods EXECUTE_ML81N_CONTINUE .
  methods EXECUTE_ML81N_SAVE .
  methods EXECUTE_ML81N_DELETE .
  methods CALL_SHDB_ML81N_REVOKE
    importing
      !IV_ENTRYSHEET type BAPIESSR-SHEET_NO
    returning
      value(RT_SHDB_MESSAGES) type TAB_BDCMSGCOLL .
  methods EXECUTE_SAVE_POASSIGN .
  methods EXECUTE_PREREQ_MIGO
    importing
      !IV_PREREQ_VALUE type ABAP_BOOL optional
      !IV_NO_SUCCESS_MSG type ABAP_BOOL optional
    raising
      /TCSR/CX_EXCEPTION .
  methods EXECUTE_PREREQ_ML81N
    importing
      !IV_NO_SUCCESS_MSG type ABAP_BOOL optional
      !IV_PREREQ_VALUE type ABAP_BOOL optional
    exporting
      !EV_PREREQ_ERROR type ABAP_BOOL .
  methods EXECUTE_VALIDATE_SAVE .
  methods EXECUTE_VALIDATE_CONTINUE .
  methods EXECUTE_VALIDATE_UNDO .
  methods EXECUTE_MR8M_REVERSE .
  methods CALL_REFRESH_BAPIS .
  methods SEND_EMAIL_CANCEL .
  methods SEND_EMAIL_STEP .
  methods EXECUTE_UNDO_STEP .
  methods EXECUTE_UNDO_PROCDET .
  methods EXECUTE_STOP_PROCESS_CONTINUE .
  methods EXECUTE_STOP_PROCESS .
  methods EXECUTE_CLOSE_PROCESS .
  methods EXECUTE_CANCEL_PROCESS .
  methods SELECT_PROCFLOW .
  methods SET_STEPSTATUS_ACT_TABLE
    importing
      !IV_STEP type /TCSR/E_STEP
      !IV_STEPSTATUS type /TCSR/E_STEPSTATUS
      !IV_LASTSTEP type CHAR1 optional
      !IV_ROLLBACK type XFELD optional .
  methods SET_STEPSTATUS_STA_TABLE
    importing
      !IV_STEPSTATUS type /TCSR/E_STEPSTATUS .
  methods SAVE_TO_DB .
  methods SET_PROCTYP
    importing
      !IV_LIFNR type LFA1-LIFNR
      !IV_EBELN type EKKO-EBELN
      !IV_EBELP type EKPO-EBELP
    returning
      value(RV_PROCTYP) type STRING .
  class-methods CHK_QTY_UNIT_FILL
    importing
      !IV_PSTYP type EKPO-PSTYP
      !IV_LEBRE type EKPO-LEBRE
      !IV_VRTKZ type EKPO-VRTKZ
    returning
      value(RE_FILL) type ABAP_BOOL .
  methods FILL_PO_LIST_ASSIG .
ENDCLASS.



CLASS ZCL_NFSE_INBOUND_V1 IMPLEMENTATION.


  METHOD CALL_REFRESH_BAPIS.

    FIELD-SYMBOLS: <fs_clear> TYPE ANY TABLE.

    ASSIGN ('(SAPLEINR)READ_DATA[]') TO <fs_clear>.
    IF <fs_clear> IS ASSIGNED.
      CLEAR: <fs_clear>[].
    ENDIF.
    UNASSIGN <fs_clear>.

    ASSIGN ('(SAPLJ1BI)GT_EKBE[]') TO <fs_clear>.
    IF <fs_clear> IS ASSIGNED.
      CLEAR: <fs_clear>[].
    ENDIF.
    UNASSIGN <fs_clear>.

    FIELD-SYMBOLS: <fs_mebex> TYPE any.
    ASSIGN ('(SAPLJ1BI)T_MEBEX') TO <fs_mebex>.
    IF <fs_mebex> IS ASSIGNED.
      CLEAR: <fs_mebex>.
    ENDIF.

    FIELD-SYMBOLS: <fs_storno> TYPE any.
    ASSIGN ('(SAPLJ1BI)STORNO_FLAG') TO <fs_storno>.
    IF <fs_mebex> IS ASSIGNED.
      CLEAR: <fs_storno>.
    ENDIF.

    CALL FUNCTION 'MS_RESET_STORAGE_LIMITS'.

  ENDMETHOD.


  METHOD CALL_SHDB_ML81N_REVOKE.

    DATA:
      lt_bdc TYPE TABLE OF bdcdata.
    DATA:
      lw_bdc        TYPE bdcdata,
      lw_ctu_params TYPE ctu_params.
*------------------------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=SELP'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0340'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ENTE'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'RM11R-LBLNI'.
    lw_bdc-fval     = iv_entrysheet.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=AKCH'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ACCR'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=SAVE'.
    APPEND lw_bdc TO lt_bdc.
**-------------------------------
*    "Popup confirm
*    CLEAR: lw_bdc.
*    lw_bdc-dynbegin = 'X'.
*    lw_bdc-program  = 'SAPLSPO1'.
*    lw_bdc-dynpro   = '0300'.
*    APPEND lw_bdc TO lt_bdc.
*
*    CLEAR: lw_bdc.
*    lw_bdc-fnam     = 'BDC_OKCODE'.
*    lw_bdc-fval     = '=YES'.
*    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0110'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ENTE'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------

    lw_ctu_params-dismode = 'S'.
    lw_ctu_params-updmode = 'N'.

    CALL TRANSACTION 'ML81N'
               USING lt_bdc
             OPTIONS FROM lw_ctu_params
            MESSAGES INTO rt_shdb_messages.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD CHECK_CALL_UNDO_STEP.

    DATA:
      mw_sta_prev TYPE /tcsr/t_sta,
      mw_sta_next TYPE /tcsr/t_sta.
*------------------------------------------------

    "Check if was send action: UNDO STEP
    IF me->mv_action EQ /tcsr/c_constants=>mc_action-undo.

      "Get Step to be UNDO (current or previous Step)
      me->get_undo_step(
              EXPORTING
                iv_exec_step  = iv_call_step
              IMPORTING
                ev_undo_step  = me->mv_exec_step
                ew_prev_sta   = mw_sta_prev
                ew_next_sta   = mw_sta_next ).

    ELSE.
      "STEP to Start (normal process)
      me->mv_exec_step   = iv_call_step.
    ENDIF.

  ENDMETHOD.


  METHOD CHK_QTY_UNIT_FILL.

    " This method is meant to avoid validation in

    IF ( iv_pstyp = '9' AND iv_lebre IS INITIAL ) OR
       iv_pstyp = '1'
       OR
       ( iv_vrtkz = '3'
         AND iv_lebre IS INITIAL ).

      RETURN.

    ENDIF.

    re_fill = abap_true.
  ENDMETHOD.


  METHOD CLEAR_ATTRIBUTES.
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    mv_guid_header = i_guid.

  ENDMETHOD.


  METHOD DEQUEUE_GUID.

    CALL FUNCTION 'DEQUEUE_/TCSR/ENQ_GUID'
      EXPORTING
        mode_/tcsr/s_enq_guid = 'E'
        mandt                 = sy-mandt
        guid_header           = me->mv_guid_header
        _scope                = '3'.

  ENDMETHOD.


  METHOD ENQUEUE_GUID.

    DATA:
      lv_attr1 TYPE scx_attrname,
      lv_attr2 TYPE scx_attrname.
*-----------------------------------------------------------------------

    CALL FUNCTION 'ENQUEUE_/TCSR/ENQ_GUID'
      EXPORTING
        mode_/tcsr/s_enq_guid = 'E'
        mandt                 = sy-mandt
        guid_header           = me->mv_guid_header
        _scope                = '3'
      EXCEPTIONS
        foreign_lock          = 1
        system_failure        = 2
        OTHERS                = 3.
    IF sy-subrc NE 0.

      lv_attr1 = me->mv_guid_header.
      lv_attr2 = sy-msgv1.
      "Set Message Key
      mo_util_xml->set_msgkey( iv_msgno    = '001'  "GUID &1 in processing by &2
                               in_attr1    = lv_attr1
                               in_attr2    = lv_attr2 ).
      RAISE EXCEPTION TYPE /tcsr/cx_exception
        EXPORTING
          textid = mo_util_xml->mw_msgkey.

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_CANCEL_PROCESS.
    DATA:
      lt_lines  TYPE TABLE OF tline.
    DATA:
      lw_cancr_text TYPE tline-tdline,
      lw_lines      TYPE tline,
      lw_header     TYPE thead.
    DATA:
      lv_guid_header TYPE char32.
*-----------------------------------------------------------------------

    IF me->mt_cancr_text[] IS NOT INITIAL.

      lw_header-tdid      = 'ST'.
      lw_header-tdspras   = sy-langu.
      lw_header-tdobject  = 'TEXT'.
      lv_guid_header      = me->mv_guid_header.
      CONCATENATE 'CANCPROC' lv_guid_header INTO lw_header-tdname.

      LOOP AT me->mt_cancr_text INTO lw_cancr_text.
        MOVE lw_cancr_text TO lw_lines-tdline.
        APPEND lw_lines TO lt_lines.
      ENDLOOP.

      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          client          = sy-mandt
          header          = lw_header
          insert          = 'X'
          savemode_direct = 'X'
        TABLES
          lines           = lt_lines
        EXCEPTIONS
          id              = 1
          language        = 2
          name            = 3
          object          = 4
          OTHERS          = 5.
    ENDIF.

    CALL FUNCTION 'COMMIT_TEXT'
      EXPORTING
        savemode_direct = 'X'.


    "Set Cancelation Reason
    me->mw_act-cancreason = me->mv_cancreason.

    MESSAGE s204(/tcsr/msg) INTO me->mv_dummy.  "Processo NFS-e Cancelado manualmente
    me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-deleted ).

  ENDMETHOD.


  METHOD EXECUTE_CHECK_DOCREF.

    TYPES:
      BEGIN OF ty_essr,
        lblni TYPE essr-lblni,
        ebeln TYPE essr-ebeln,
        ebelp TYPE essr-ebelp,
      END OF ty_essr.
    TYPES:
      BEGIN OF ty_ekpo,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        bukrs TYPE ekko-bukrs,
        werks TYPE ekpo-werks,
        loekz TYPE ekko-loekz,
      END OF ty_ekpo.
    TYPES:
      BEGIN OF ty_t001w,
        werks      TYPE t001w-werks,
        j_1bbranch TYPE t001w-j_1bbranch,
      END OF ty_t001w.
    TYPES:
      BEGIN OF ty_po,
        guid_header TYPE /tcsr/t_po-guid_header,
        doc_number  TYPE /tcsr/t_po-doc_number,
      END OF ty_po.

    DATA:
      lt_essr      TYPE TABLE OF ty_essr,
      lt_ekpo      TYPE TABLE OF ty_ekpo,
      lt_ekpo_fae  TYPE TABLE OF ty_ekpo,
      lt_t001w     TYPE TABLE OF ty_t001w,
      lt_doc_po    TYPE TABLE OF /tcsr/t_proc_doc,
      lt_po        TYPE TABLE OF ty_po,
      lt_doc_sheet TYPE TABLE OF /tcsr/t_proc_doc,
      lt_doc_gr    TYPE TABLE OF /tcsr/t_proc_doc.
    DATA:
      lw_essr      TYPE ty_essr,
      lw_po        TYPE ty_po,
      lw_t001w     TYPE ty_t001w,
      lw_doc_po    TYPE /tcsr/t_proc_doc,
      lw_doc_sheet TYPE /tcsr/t_proc_doc,
      lw_doc_gr    TYPE /tcsr/t_proc_doc,
      lw_ekpo      TYPE ty_ekpo.
    DATA:
      lv_guid_header TYPE /tcsr/e_guid_header,
      lv_branch      TYPE t001w-j_1bbranch,
      lv_cgc_number  TYPE j_1bwfield-cgc_number,
      lv_lines       TYPE sy-tabix.
    FIELD-SYMBOLS:
      <fs_po> TYPE /tcsr/t_po.
*------------------------------------------------

    IF me->mt_proc_doc[] IS INITIAL.
      MESSAGE s293(/tcsr/msg) INTO me->mv_dummy.   "Nenhum documento de referência associado
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).

    ELSE.


      lt_doc_sheet[] = me->mt_proc_doc[].
      DELETE lt_doc_sheet WHERE vgabe NE '9'.

      lt_doc_gr[] = me->mt_proc_doc[].
      DELETE lt_doc_gr WHERE vgabe NE '1'.

      lt_doc_po[] = me->mt_proc_doc[].
      DELETE lt_doc_po WHERE doc_number IS NOT INITIAL.
      SORT lt_doc_po BY doc_number.

*---Service Entry Sheet filled by Vendor
      LOOP AT lt_doc_sheet INTO lw_doc_sheet.

        READ TABLE me->mt_po INTO me->mw_po WITH KEY ebeln      = lw_doc_sheet-ebeln
                                                     ebelp      = lw_doc_sheet-ebelp
                                                     doc_number = lw_doc_sheet-doc_number
                                                     extrow     = lw_doc_sheet-extrow.
        IF sy-subrc NE 0.

          "Insert PO/ITEM from SES
          APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
          MOVE lw_doc_sheet TO <fs_po>.

          me->mw_act-ref_doc_no = abap_true.

          me->mw_po_list_assig-ebeln  = <fs_po>-ebeln.
          me->mw_po_list_assig-ebelp  = <fs_po>-ebelp.
          me->mw_po_list_assig-lblni  = <fs_po>-doc_number.
          me->mw_po_list_assig-extrow = <fs_po>-extrow.
          APPEND me->mw_po_list_assig TO me->mt_po_list_assig.
        ENDIF.
        MESSAGE s282(/tcsr/msg) INTO me->mv_dummy   "Associada Folha Registro de Serviço &
                                WITH lw_doc_sheet-doc_number.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).

      ENDLOOP.


*---Goods Receipt (MIGO) filled by Vendor
      LOOP AT lt_doc_gr INTO lw_doc_gr.

        READ TABLE me->mt_po INTO me->mw_po WITH KEY ebeln      = lw_doc_gr-ebeln
                                                     ebelp      = lw_doc_gr-ebelp
                                                     doc_number = lw_doc_gr-doc_number.
        IF sy-subrc NE 0.
          "Insert PO/ITEM from SES
          APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
          MOVE lw_doc_gr TO <fs_po>.
        ENDIF.

        MESSAGE s356(/tcsr/msg) INTO me->mv_dummy   "Associado Documento de Material & &
                                WITH lw_doc_gr-doc_number lw_doc_gr-gjahr.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
        me->mw_act-ref_doc_no = abap_true.

      ENDLOOP.


*---Purchase Order Filled by Vendor
      LOOP AT lt_doc_po INTO lw_doc_po.

        READ TABLE me->mt_po INTO me->mw_po WITH KEY ebeln = lw_doc_po-ebeln
                                                     ebelp = lw_doc_po-ebelp
                                                     extrow = lw_doc_po-extrow.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
          MOVE lw_doc_po TO <fs_po>.
        ENDIF.

        MESSAGE s292(/tcsr/msg) INTO me->mv_dummy   "Pedido & e item & foram associados
                                WITH lw_doc_po-ebeln lw_doc_po-ebelp.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
      ENDLOOP.

      "Purchase Order Table
      IF me->mt_po[] IS NOT INITIAL.
        MODIFY /tcsr/t_po FROM TABLE me->mt_po.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_CLOSE_PROCESS.

    DATA: "lt_po_list   TYPE /tcsr/y_po_list,
          lt_cond_list TYPE /tcsr/y_cond_list.
    DATA: go_util_moni TYPE REF TO /tcsr/c_util_monitor,
          go_exception TYPE REF TO /tcsr/cx_exception.
    DATA: gv_total_sap_value TYPE kwert.
    FIELD-SYMBOLS: <fs_po_list>   TYPE /tcsr/s_po_list,
                   <fs_cond_list> TYPE /tcsr/s_cond_list.


    me->mw_nfstx_upd = me->mw_nfstx.

    " Create Object Utilit Monitor
    CREATE OBJECT go_util_moni
      EXPORTING
        iv_guid_header = me->mw_hd-guid_header.

    TRY .
        go_util_moni->get_xml( ).
      CATCH /tcsr/cx_exception INTO go_exception.
    ENDTRY.

    " Get Screen Condition Values (9021/9050)
    go_util_moni->get_scr_cond_list(
          EXPORTING
            iv_tolerance_tax  = me->mw_act-tolerance_tax
            iv_approve_diff   = me->mw_act-approve_diff
          IMPORTING
            et_scr_cond_list  = lt_cond_list[]
          CHANGING
            it_po_list        = me->mt_po_list_assig[] ).

    "Return Total SAP Value
    LOOP AT me->mt_po_list_assig ASSIGNING <fs_po_list>.
      IF <fs_po_list>-value_inverted IS INITIAL.
        gv_total_sap_value = gv_total_sap_value + <fs_po_list>-dmbtr_iv.
      ELSE.
        gv_total_sap_value = gv_total_sap_value + <fs_po_list>-menge_iv.
      ENDIF.
    ENDLOOP.
    me->mw_nfstx_upd-totval_sap   = gv_total_sap_value.

    LOOP AT lt_cond_list ASSIGNING <fs_cond_list>.
      CASE <fs_cond_list>-txcode .
        WHEN 'PIS'.
          me->mw_nfstx_upd-rate_pis     = <fs_cond_list>-sap_rate.
          me->mw_nfstx_upd-value_pis    = <fs_cond_list>-sap_value.
        WHEN 'COFINS'.
          me->mw_nfstx_upd-rate_cofins  = <fs_cond_list>-sap_rate.
          me->mw_nfstx_upd-value_cofins = <fs_cond_list>-sap_value.
        WHEN 'INSS'.
          me->mw_nfstx_upd-rate_inss    = <fs_cond_list>-sap_rate.
          me->mw_nfstx_upd-value_inss   = <fs_cond_list>-sap_value.
        WHEN 'IR'.
          me->mw_nfstx_upd-rate_ir      = <fs_cond_list>-sap_rate.
          me->mw_nfstx_upd-value_ir     = <fs_cond_list>-sap_value.
        WHEN 'CSLL'.
          me->mw_nfstx_upd-rate_csll    = <fs_cond_list>-sap_rate.
          me->mw_nfstx_upd-value_csll   = <fs_cond_list>-sap_value.
        WHEN 'ISS'.
          me->mw_nfstx_upd-rate_iss     = <fs_cond_list>-sap_rate.
          me->mw_nfstx_upd-value_iss    = <fs_cond_list>-sap_value.
        WHEN 'PISCOFCSLL'.
          me->mw_nfstx_upd-rate_piscofcsll  = <fs_cond_list>-sap_rate.
          me->mw_nfstx_upd-value_piscofcsll = <fs_cond_list>-sap_value.
      ENDCASE.
    ENDLOOP.


    CASE me->mw_sta-autoproc.
      WHEN /tcsr/c_constants=>c_auto_step.
        MESSAGE s421(/tcsr/msg) "NFS-e Process Closed automatically
           INTO me->mv_dummy.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-closed ).
      WHEN /tcsr/c_constants=>c_manual_step.
        MESSAGE s422(/tcsr/msg)
           WITH sy-uname "NFS-e Process Closed by &
           INTO me->mv_dummy.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-closed ).
    ENDCASE.

  ENDMETHOD.


  METHOD EXECUTE_DOCREF.

    TYPES: BEGIN OF ty_ekpo,
             ebeln  TYPE ekpo-ebeln,
             ebelp  TYPE ekpo-ebelp,
             packno TYPE ekpo-packno,
             loekz  TYPE ekpo-loekz,
           END OF ty_ekpo,

           BEGIN OF ty_essr,
             lblni  TYPE essr-lblni,
             packno TYPE essr-packno,
             ebeln  TYPE essr-ebeln,
             ebelp  TYPE essr-ebelp,
           END OF ty_essr,

           BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             mjahr TYPE mseg-mjahr,
             zeile TYPE mseg-zeile,
             ebeln TYPE mseg-ebeln,
             ebelp TYPE mseg-ebelp,
             menge TYPE mseg-menge,
             dmbtr TYPE mseg-dmbtr,
           END OF ty_mseg.

    DATA:
      lt_docs      TYPE TABLE OF /tcsr/t_po,
      lt_docs_ref  TYPE TABLE OF /tcsr/t_po,
      lt_docs_fae  TYPE TABLE OF /tcsr/t_po,
      lt_ekpo      TYPE TABLE OF ty_ekpo,
      lt_essr      TYPE TABLE OF ty_essr,
      lt_mseg      TYPE TABLE OF ty_mseg,
      lt_esll_pckg TYPE TABLE OF esll,
      lt_esll      TYPE TABLE OF esll.
    DATA:
      lw_log_handle TYPE balloghndl,
      lw_ekpo       TYPE ty_ekpo,
      lw_essr       TYPE ty_essr,
      lw_mseg       TYPE ty_mseg,
      lw_esll_pckg  TYPE esll,
      lw_esll       TYPE esll.
    DATA:
      lv_error TYPE c,
      lv_tabix TYPE i.
    FIELD-SYMBOLS:
      <fs_return> TYPE bapiret2,
      <fs_docs>   TYPE /tcsr/t_po.
    DATA:
      lo_prereq_ref TYPE REF TO /tcsr/c_prereq_ref,
      lo_bal_log     TYPE REF TO /tcsr/c_bal_log.

    IF me->mt_ref[] IS INITIAL.
      MESSAGE e298(/tcsr/msg) "Not found any reference document
         INTO me->mv_dummy.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
      EXIT.
    ENDIF.

    "Get documents informed by vendor
    LOOP AT me->mt_ref INTO me->mw_ref.

      APPEND INITIAL LINE TO lt_docs_ref ASSIGNING <fs_docs>.

      <fs_docs>-guid_header = me->mv_guid_header.

      IF me->mw_ref-doc_type = 'S'.
        <fs_docs>-vgabe       = '9'.
        <fs_docs>-doc_number  = me->mw_ref-doc_number.
      ELSEIF me->mw_ref-doc_type = 'G'.
        <fs_docs>-vgabe       = '1'.
        <fs_docs>-doc_number  = me->mw_ref-doc_number.
        <fs_docs>-gjahr       = me->mw_ref-gjahr.
      ELSEIF me->mw_ref-doc_type = 'P'.
        <fs_docs>-ebeln       = me->mw_ref-doc_number.
      ENDIF.
    ENDLOOP.


    "Get PO related to SES
    lt_docs_fae[] = lt_docs_ref[].
    DELETE lt_docs_fae WHERE vgabe NE '9'.
    IF lt_docs_fae[] IS NOT INITIAL.
      SELECT lblni
             packno
             ebeln
             ebelp
        INTO TABLE lt_essr
        FROM essr
        FOR ALL ENTRIES IN lt_docs_fae
        WHERE lblni = lt_docs_fae-doc_number.
      IF sy-subrc = 0.
        SORT lt_essr BY lblni.

        "Select Sevices Header
        SELECT *
          APPENDING TABLE lt_esll_pckg
          FROM esll
          FOR ALL ENTRIES IN lt_essr
          WHERE packno = lt_essr-packno.
        IF sy-subrc EQ 0.

          "Select Services Item
          SELECT *
            APPENDING TABLE lt_esll
            FROM esll
            FOR ALL ENTRIES IN lt_esll_pckg
            WHERE packno = lt_esll_pckg-sub_packno.
        ENDIF.

        "Remove from reference table SES's PO if it was informed in NFS-e too.
        LOOP AT lt_docs_ref ASSIGNING <fs_docs>.
          lv_tabix = sy-tabix.
          READ TABLE lt_essr TRANSPORTING NO FIELDS WITH KEY ebeln = <fs_docs>-ebeln.
          IF sy-subrc = 0.
            DELETE lt_docs_ref INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.



    "Get PO items
    lt_docs_fae[] = lt_docs_ref[].
    DELETE lt_docs_fae WHERE ebeln IS INITIAL.
    IF lt_docs_fae[] IS NOT INITIAL.

      SELECT ebeln
             ebelp
             packno
             loekz
        INTO TABLE lt_ekpo
        FROM ekpo
        FOR ALL ENTRIES IN lt_docs_fae
        WHERE ebeln = lt_docs_fae-ebeln.
      DELETE lt_ekpo WHERE loekz IS NOT INITIAL.
      IF lt_ekpo[] IS NOT INITIAL.
        SORT lt_ekpo BY ebeln.

        "Select Sevices Header
        SELECT *
          APPENDING TABLE lt_esll_pckg
          FROM esll
          FOR ALL ENTRIES IN lt_ekpo
          WHERE packno = lt_ekpo-packno.
        IF sy-subrc EQ 0.

          "Select Services Item
          SELECT *
            APPENDING TABLE lt_esll
            FROM esll
            FOR ALL ENTRIES IN lt_esll_pckg
            WHERE packno = lt_esll_pckg-sub_packno.

        ENDIF.

      ENDIF.
    ENDIF.



    "Get PO related to MIGO
    lt_docs_fae[] = lt_docs_ref[].
    DELETE lt_docs_fae WHERE vgabe NE '1'.
    IF lt_docs_fae[] IS NOT INITIAL.
      SELECT mblnr
             mjahr
             zeile
             ebeln
             ebelp
             menge
             dmbtr
        INTO TABLE lt_mseg
        FROM mseg
        FOR ALL ENTRIES IN lt_docs_fae
        WHERE mblnr = lt_docs_fae-doc_number
          AND mjahr = lt_docs_fae-gjahr.
      SORT lt_mseg BY mblnr mjahr.
    ENDIF.


    SORT lt_esll_pckg BY packno.
    SORT lt_esll BY packno.


    "--------------------------------------------
    " PRE-REQs for documents
    "--------------------------------------------
    IF lt_docs_ref[] IS NOT INITIAL.
      CREATE OBJECT lo_prereq_ref.
      lo_prereq_ref->load_new_guid( EXPORTING iw_hd    = me->mw_hd
                                              iw_act   = me->mw_act
                                              it_docs  = lt_docs_ref[] ).

      LOOP AT lt_docs_ref ASSIGNING <fs_docs>.

        CLEAR lv_error.

        lo_prereq_ref->load_new_doc( EXPORTING iw_docs  = <fs_docs> ).


        "SES ASSIGNED
        IF <fs_docs>-vgabe EQ '9'.

          IF lo_prereq_ref->check_ses_exist( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
          IF lo_prereq_ref->check_ses_another_nfse( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
          IF lo_prereq_ref->check_ses_deleted( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
          IF lo_prereq_ref->check_po_ses_bukrs( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.

          "Get all service lines
          IF lv_error IS INITIAL.
            READ TABLE lt_essr INTO lw_essr WITH KEY lblni = <fs_docs>-doc_number
                                                                    BINARY SEARCH.
            IF sy-subrc = 0.

              "Read Header Packno
              READ TABLE lt_esll_pckg INTO lw_esll_pckg WITH KEY packno = lw_essr-packno
                                                                           BINARY SEARCH.
              CHECK: sy-subrc EQ 0.

              "Read Services Item
              READ TABLE lt_esll TRANSPORTING NO FIELDS WITH KEY packno = lw_esll_pckg-sub_packno
                                                                                    BINARY SEARCH.
              IF sy-subrc EQ 0.

                "Include all service lines to T_PO
                LOOP AT lt_esll INTO lw_esll FROM sy-tabix.
                  IF lw_esll-packno NE lw_esll_pckg-sub_packno.
                    EXIT.
                  ENDIF.
                  <fs_docs>-ebeln    = lw_essr-ebeln.
                  <fs_docs>-ebelp    = lw_essr-ebelp.
                  <fs_docs>-extrow   = lw_esll-extrow.
                  <fs_docs>-menge    = lw_esll-menge.
                  <fs_docs>-dmbtr    = lw_esll-menge * lw_esll-tbtwr.
                  APPEND <fs_docs> TO lt_docs.
                ENDLOOP.
              ENDIF.
            ENDIF.

          ENDIF.


          "MIGO ASSIGNED
        ELSEIF <fs_docs>-vgabe EQ '1'.

          IF lo_prereq_ref->check_gr_exist( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
          IF lo_prereq_ref->check_gr_another_nfse( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
          IF lo_prereq_ref->check_gr_bukrs( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.

          IF lv_error IS INITIAL.
            READ TABLE lt_mseg INTO lw_mseg WITH KEY mblnr = <fs_docs>-doc_number
                                                     mjahr = <fs_docs>-gjahr
                                                                    BINARY SEARCH.
            IF sy-subrc = 0.
              <fs_docs>-ebeln = lw_mseg-ebeln.
              <fs_docs>-ebelp = lw_mseg-ebelp.
              <fs_docs>-menge = lw_mseg-menge.
              <fs_docs>-dmbtr = lw_mseg-dmbtr.
              APPEND <fs_docs> TO lt_docs.
            ENDIF.
          ENDIF.


          "PO assigned
        ELSEIF <fs_docs>-ebeln IS NOT INITIAL.

          IF lo_prereq_ref->check_po_exist( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
          IF lo_prereq_ref->check_po_bukrs( ) IS NOT INITIAL.   "+CYS 02.02.2022
            lv_error = abap_true.                               "+CYS 02.02.2022
          ENDIF.                                                "+CYS 02.02.2022
          IF lo_prereq_ref->check_po_deleted( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.
          IF lo_prereq_ref->check_po_locked( ) IS NOT INITIAL.
            lv_error = abap_true.
          ENDIF.

          IF lv_error IS INITIAL.

            "Include item number
            LOOP AT lt_ekpo INTO lw_ekpo WHERE ebeln = <fs_docs>-ebeln.

              "Read Header Packno
              READ TABLE lt_esll_pckg INTO lw_esll_pckg WITH KEY packno = lw_ekpo-packno
                                                                          BINARY SEARCH.
              CHECK: sy-subrc EQ 0.

              "Read Services Item
              READ TABLE lt_esll TRANSPORTING NO FIELDS WITH KEY packno = lw_esll_pckg-sub_packno
                                                                                    BINARY SEARCH.
              IF sy-subrc EQ 0.

                "Include all service lines to T_PO
                LOOP AT lt_esll INTO lw_esll FROM sy-tabix.
                  IF lw_esll-packno NE lw_esll_pckg-sub_packno.
                    EXIT.
                  ENDIF.
                  <fs_docs>-ebelp    = lw_ekpo-ebelp.
                  <fs_docs>-extrow   = lw_esll-extrow.
                  <fs_docs>-menge    = lw_esll-menge.
                  <fs_docs>-dmbtr    = lw_esll-menge * lw_esll-tbtwr.
                  APPEND <fs_docs> TO lt_docs.
                ENDLOOP.
              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDLOOP.
    ENDIF.


    "--------------------------------------------
    " SET Update Table
    "--------------------------------------------
    IF lt_docs[] IS NOT INITIAL.
      me->mt_po[] = lt_docs[].
      me->mt_po_upd[] = me->mt_po[].
      MESSAGE s284(/tcsr/msg) "Assigned document from NFS-e
         INTO me->mv_dummy.
      me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-success ).
    ELSE.

      IF lo_prereq_ref->mt_return_error[] IS NOT INITIAL.

        "===================================================================
        " CHECK: Return Error
        "===================================================================

        CREATE OBJECT lo_bal_log.

        CALL METHOD lo_bal_log->bal_log_create
          EXPORTING
            iv_guid_header = me->mw_hd-guid_header
          IMPORTING
            ew_log_handle  = lw_log_handle.

        "--------------------------------------------
        " SAVE: Return Error Messages
        "--------------------------------------------
        LOOP AT lo_prereq_ref->mt_return_error ASSIGNING <fs_return>.
          MESSAGE ID <fs_return>-id
             TYPE <fs_return>-type
           NUMBER <fs_return>-number
             WITH <fs_return>-message_v1
                  <fs_return>-message_v2
                  <fs_return>-message_v3
                  <fs_return>-message_v4
             INTO me->mv_dummy.
          CALL METHOD lo_bal_log->bal_log_msg_add
            EXPORTING
              iw_log_handle = lw_log_handle.
        ENDLOOP.

        CALL METHOD lo_bal_log->bal_db_save
          EXPORTING
            iw_log_handle = lw_log_handle.

      ENDIF.

      MESSAGE w293(/tcsr/msg) "No reference document assigned (see detailed log)
                INTO me->mv_dummy.
      me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-warning ).
    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_DOWNLOAD_CONTINUE.

    DATA:
      lw_wsfile     TYPE /tcsr/t_wsfile,
      lw_log_handle TYPE balloghndl.
*-----------------------------------------------------------------------

    "Check table Waiting for Download
    SELECT SINGLE *
      INTO lw_wsfile
      FROM /tcsr/t_wsfile
      WHERE guid_header = me->mv_guid_header.
    IF sy-subrc EQ 0.

      CASE lw_wsfile-status.

        WHEN ''.
          MESSAGE s211(/tcsr/msg) INTO me->mv_dummy. "Aguardando conversão e download do arquivo XML
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-pending ).

        WHEN 'E'.

          MESSAGE ID lw_wsfile-msgid
             TYPE lw_wsfile-msgty
           NUMBER lw_wsfile-msgno
             WITH lw_wsfile-msgv1 lw_wsfile-msgv2 lw_wsfile-msgv3 lw_wsfile-msgv4
             INTO me->mv_dummy.

          "Check Same message - Save Bal Log
          IF lw_wsfile-msgid = me->mw_sta-msgid
         AND lw_wsfile-msgty = me->mw_sta-msgty
         AND lw_wsfile-msgno = me->mw_sta-msgno.

            "Create BAL LOG
            CALL METHOD mo_bal_log->bal_log_create
              EXPORTING
                iv_guid_header = me->mv_guid_header
              IMPORTING
                ew_log_handle  = lw_log_handle.
            "Add Bal Log Msg
            CALL METHOD mo_bal_log->bal_log_msg_add
              EXPORTING
                iw_log_handle = lw_log_handle.
            "Save Bal Log
            CALL METHOD mo_bal_log->bal_db_save
              EXPORTING
                iw_log_handle = lw_log_handle.

          ELSE.
            me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
          ENDIF.

        WHEN 'X'.   "Technical Error
          MESSAGE e212(/tcsr/msg) INTO me->mv_dummy.  "Erro ao executar download do arquivo XML
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).

        WHEN 'S'.   "XML identificado no WS NDD e atualizado no SAP
          MESSAGE s213(/tcsr/msg) INTO me->mv_dummy.  "Download do arquivo XML efetuado com sucesso
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).

          "Update Current Status Table(Filename/Download date and time)
          me->mw_act-filename   = lw_wsfile-filename.
          me->mw_act-dwdat      = lw_wsfile-dwdat.
          me->mw_act-dwtim      = lw_wsfile-dwtim.

          "Append WSFile to be DELETED
*          APPEND lw_wsfile TO me->mt_wsfile_del.

      ENDCASE.

    ELSE.

      MESSAGE s211(/tcsr/msg) INTO me->mv_dummy.  "Aguardando conversão e download do arquivo XML
      me->set_stepstatus_sta_table( EXPORTING iv_stepstatus = /tcsr/c_constants=>mc_status-error ).

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_JOB_PROCSTEP.

    DATA:
      lv_numero   TYPE char15,
      lv_jobname  TYPE tbtcjob-jobname,
      lv_jobcount TYPE tbtcjob-jobcount.
*----------------------------------------------------------

    lv_numero = me->mw_hd-nfse_numero.
    SHIFT lv_numero LEFT DELETING LEADING '0'.

    CONCATENATE me->mv_job_name  "/TCSR/PROCSTEP
                '-'
                lv_numero
           INTO lv_jobname.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    CHECK: sy-subrc EQ 0.


    SUBMIT /tcsr/r_process_steps
           VIA JOB lv_jobname
           NUMBER lv_jobcount
           WITH p_guid   EQ me->mw_hd-guid_header
           WITH p_action EQ /tcsr/c_constants=>mc_action-continue
           WITH p_lstep  EQ me->mw_sta-step
           USER me->mv_job_user
           AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = lv_jobname
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        OTHERS               = 9.

  ENDMETHOD.


  METHOD EXECUTE_MIGO_CONTINUE.

    DATA:
      lv_mblnr            TYPE mblnr,
      lv_mjahr            TYPE mjahr,
      lv_create_migo_auto TYPE c.

    FIELD-SYMBOLS:
      <fs_po_upd> TYPE /tcsr/t_po,
      <fs_po_del> TYPE /tcsr/t_po,
      <fs_po>     TYPE /tcsr/t_po.
    DATA:
      lo_exception TYPE REF TO /tcsr/cx_exception,
      lo_param     TYPE REF TO /tcsr/c_param.

    CREATE OBJECT lo_param.
    lv_create_migo_auto   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                     iv_name     = 'CREATE_MIGO_AUTO' ).
    IF lv_create_migo_auto IS INITIAL.
      me->mv_not_create = abap_true.
    ENDIF.

    "Verify if at least PO was informed
    IF me->mt_po[] IS INITIAL.
      MESSAGE s293(/tcsr/msg) "No reference document assigned
         INTO me->mv_dummy.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
      EXIT.
    ENDIF.

    "Verify if all items have MIGO assigned to
    LOOP AT me->mt_po ASSIGNING <fs_po>.
      IF <fs_po>-doc_number IS INITIAL.

        IF  me->mv_not_create IS INITIAL.    "Create MIGO only if user has accepted

          TRY.
              "Execute prerequisites for MIGO
              me->execute_prereq_migo( EXPORTING iv_prereq_value = abap_true ).

            CATCH /tcsr/cx_exception INTO lo_exception.
              "Returned Error
              MESSAGE ID lo_exception->if_t100_message~t100key-msgid
                 TYPE 'E'
               NUMBER lo_exception->if_t100_message~t100key-msgno
                 WITH lo_exception->if_t100_message~t100key-attr1
                      lo_exception->if_t100_message~t100key-attr2
                      lo_exception->if_t100_message~t100key-attr3
                      lo_exception->if_t100_message~t100key-attr4
                 INTO me->mv_dummy.
              me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
              EXIT.
          ENDTRY.

*          me->execute_migo_create( EXPORTING iv_ebeln = <fs_po>-ebeln
*                                             iv_ebelp = <fs_po>-ebelp
*                                   IMPORTING ev_mblnr = lv_mblnr
*                                             ev_mjahr = lv_mjahr ).

          IF lv_mblnr IS NOT INITIAL.

            MESSAGE s360(/tcsr/msg) "Documento de Material & & registrado
               INTO me->mv_dummy WITH lv_mblnr lv_mjahr.
            me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).

            "Update MIGO created
            APPEND INITIAL LINE TO me->mt_po_upd ASSIGNING <fs_po_upd>.
            <fs_po_upd>-guid_header = me->mv_guid_header.
            <fs_po_upd>-ebeln       = <fs_po>-ebeln.
            <fs_po_upd>-ebelp       = <fs_po>-ebelp.
            <fs_po_upd>-vgabe      = '1'.
            <fs_po_upd>-doc_number = lv_mblnr.
            <fs_po_upd>-gjahr      = lv_mjahr.

            <fs_po>-doc_number = lv_mblnr.
            <fs_po>-gjahr      = lv_mjahr.
            <fs_po>-vgabe      = '1'.

            "Delete entry without entry sheet
            APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
            <fs_po_del>-guid_header = me->mv_guid_header.
            <fs_po_del>-ebeln       = <fs_po>-ebeln.
            <fs_po_del>-ebelp       = <fs_po>-ebelp.

            "Execute prerequisites for MIGO
            me->execute_prereq_migo( ).

          ENDIF.

        ELSE.
          MESSAGE s351(/tcsr/msg) "Material Document was not assigned
             INTO me->mv_dummy.
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
          EXIT.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD EXECUTE_MIGO_CREATE.

    TYPES: BEGIN OF ty_ekko,
             ebeln TYPE ekko-ebeln,
             lifnr TYPE ekko-lifnr,
           END OF ty_ekko.
    TYPES: BEGIN OF ty_ekpo,
             ebeln TYPE ekpo-ebeln,
             ebelp TYPE ekpo-ebelp,
             matnr TYPE ekpo-matnr,
             werks TYPE ekpo-werks,
             lgort TYPE ekpo-lgort,
             menge TYPE ekpo-menge,
             meins TYPE ekpo-meins,
           END OF ty_ekpo.
    TYPES: BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             mjahr TYPE mseg-mjahr,
             zeile TYPE mseg-zeile,
             ebeln TYPE mseg-ebeln,
             ebelp TYPE mseg-ebelp,
           END OF ty_mseg.
    DATA:
      lt_ekpo          TYPE TABLE OF ty_ekpo,
      lt_mseg          TYPE TABLE OF ty_mseg,
      lt_goodsmvt_item TYPE TABLE OF bapi2017_gm_item_create,
      "lt_return        TYPE TABLE OF bapiret2,
      lt_po_list_assig TYPE /tcsr/y_po_list.
    DATA:
      lw_goodsmvt_header TYPE bapi2017_gm_head_01,
      lw_goodsmvt_code   TYPE bapi2017_gm_code,
      lw_head_ret        TYPE bapi2017_gm_head_ret,
      lw_log_handle      TYPE balloghndl.
    DATA:
      lv_lifnr    TYPE ekko-lifnr,
      lv_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
      lv_doc_year TYPE bapi2017_gm_head_ret-doc_year,
      lv_line     TYPE bapi2017_gm_item_create-line_id,
      lv_lines    TYPE i.
    FIELD-SYMBOLS:
      <fs_po_list>       TYPE /tcsr/s_po_list,
      <fs_ekpo>          TYPE ty_ekpo,
      <fs_mseg>          TYPE ty_mseg,
      <fs_goodsmvt_item> TYPE bapi2017_gm_item_create,
      <fs_return>        TYPE bapiret2,
      <fs_po_upd>        TYPE /tcsr/t_po.
    DATA:
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.
*------------------------------------------------

    me->mt_po_list_assig[] = it_po_list[].

    CHECK: me->mt_po_list_assig[] IS NOT INITIAL.

    lt_po_list_assig[] = me->mt_po_list_assig.
    DELETE lt_po_list_assig WHERE mblnr IS NOT INITIAL.

    "Select Purchase Items
    SELECT ebeln ebelp matnr werks lgort menge meins
      FROM ekpo
      INTO TABLE lt_ekpo
      FOR ALL ENTRIES IN lt_po_list_assig
      WHERE ebeln = lt_po_list_assig-ebeln
        AND ebelp = lt_po_list_assig-ebelp.
    CHECK: sy-subrc EQ 0.

    SORT lt_ekpo BY ebeln ebelp.

    READ TABLE me->mt_po_list_assig ASSIGNING <fs_po_list> INDEX 1.
    CHECK: sy-subrc EQ 0.

    "Select Vendor from Purchase Order
    SELECT SINGLE lifnr
      INTO lv_lifnr
      FROM ekko
     WHERE ebeln = <fs_po_list>-ebeln.
    CHECK: sy-subrc EQ 0.


    "--------------------------------------------
    " FILL BAPI STRUCTURES
    "--------------------------------------------

* Header
    lw_goodsmvt_header-doc_date         = sy-datlo.
    lw_goodsmvt_header-pstng_date       = sy-datlo.
    lw_goodsmvt_header-header_txt       = me->mw_hd-nfse_numero.
    lw_goodsmvt_header-pr_uname         = sy-uname.
    lw_goodsmvt_code-gm_code            = '01'.                   "Goods movement code

    CLEAR: lv_line.

    LOOP AT me->mt_po_list_assig ASSIGNING <fs_po_list>.

      READ TABLE lt_ekpo ASSIGNING <fs_ekpo>
                         WITH KEY ebeln = <fs_po_list>-ebeln
                                  ebelp = <fs_po_list>-ebelp
                                  BINARY SEARCH.
      CHECK: sy-subrc EQ 0.

      APPEND INITIAL LINE TO lt_goodsmvt_item ASSIGNING <fs_goodsmvt_item>.

"*---> 01/07/2023 - Migração S4 - LO
*      <fs_goodsmvt_item>-material       = <fs_ekpo>-matnr.
   DATA(v_len) = strlen( <fs_ekpo>-matnr ).

   IF v_len > 18.
     <fs_goodsmvt_item>-material_long = <fs_ekpo>-matnr.
   ELSE.
     <fs_goodsmvt_item>-material      = <fs_ekpo>-matnr.
   ENDIF.
"*---> 01/07/2023 - Migração S4 - LO
      <fs_goodsmvt_item>-plant          = <fs_ekpo>-werks.
      <fs_goodsmvt_item>-stge_loc       = <fs_ekpo>-lgort.
      <fs_goodsmvt_item>-move_type      = '101'.                  "Set Movement ID
      <fs_goodsmvt_item>-vendor         = lv_lifnr.               "Set Vendor
      <fs_goodsmvt_item>-entry_qnt      = <fs_po_list>-menge_iv.  "Qtd Filled
      <fs_goodsmvt_item>-entry_uom      = <fs_ekpo>-meins.
      <fs_goodsmvt_item>-entry_uom_iso  = <fs_ekpo>-meins.
      <fs_goodsmvt_item>-po_pr_qnt      = <fs_po_list>-menge_iv.  "Qtd Filled
      <fs_goodsmvt_item>-orderpr_un     = <fs_ekpo>-meins.
      <fs_goodsmvt_item>-orderpr_un_iso = <fs_ekpo>-meins.

      "Set PO/ITEM
      <fs_goodsmvt_item>-po_number      = <fs_ekpo>-ebeln.
      <fs_goodsmvt_item>-po_item        = <fs_ekpo>-ebelp.
      <fs_goodsmvt_item>-mvt_ind        = 'B'.                    "Movimento de mercadoria por pedido
      <fs_goodsmvt_item>-line_id        = lv_line = lv_line + 1.

    ENDLOOP.

    CLEAR mt_mess.

"*---> 01/07/2023 - Migração S4 - LO
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = lw_goodsmvt_header
        goodsmvt_code    = lw_goodsmvt_code
      IMPORTING
        goodsmvt_headret = lw_head_ret
        materialdocument = lv_mat_doc
        matdocumentyear  = lv_doc_year
      TABLES
        goodsmvt_item    = lt_goodsmvt_item
        return           = mt_mess.

    IF lv_mat_doc IS NOT INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 5 TIMES.
        "Select Detail of Invoice document Created
        SELECT mblnr
               mjahr
               zeile
               ebeln
               ebelp
          FROM mseg
          INTO TABLE lt_mseg
         WHERE mblnr = lv_mat_doc
           AND mjahr = lv_doc_year.
        IF sy-subrc EQ 0.
          EXIT.
        ENDIF.
        WAIT UP TO 1 SECONDS.
      ENDDO.
      IF sy-subrc EQ 0.

        ev_mblnr = lv_mat_doc.
        ev_mjahr = lv_doc_year.

        "Doc.material & & registrado
        MESSAGE s360(/tcsr/msg) INTO me->mv_dummy WITH lv_mat_doc lv_doc_year.
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-success ).

      ENDIF.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.

    message_format( ).

  ENDMETHOD.


  METHOD EXECUTE_MIGO_REVERSE.

    TYPES: BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             mjahr TYPE mseg-mjahr,
             zeile TYPE mseg-zeile,
             ebeln TYPE mseg-ebeln,
             ebelp TYPE mseg-ebelp,
           END OF ty_mseg.
    DATA:
      lt_mseg   TYPE TABLE OF ty_mseg,
      lt_return TYPE TABLE OF bapiret2.
    DATA:
      lw_head_ret   TYPE bapi2017_gm_head_ret,
      lw_mseg       TYPE ty_mseg,
      lw_log_handle TYPE balloghndl.
    FIELD-SYMBOLS:
      <fs_po_list> TYPE /tcsr/s_po_list,
      <fs_po_del>  TYPE /tcsr/t_po,
      <fs_return>  TYPE bapiret2.
    DATA:
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.
*------------------------------------------------

    READ TABLE me->mt_po_list_unass ASSIGNING <fs_po_list> INDEX 1.
    IF sy-subrc NE 0 OR
       ( sy-subrc EQ 0 AND <fs_po_list>-mblnr IS INITIAL ).

      "Documento de Material não foi associado
      MESSAGE e351(/tcsr/msg) INTO me->mv_dummy.

      CREATE OBJECT lo_bal_log.

      "Create BAL LOG
      CALL METHOD lo_bal_log->bal_log_create
        EXPORTING
          iv_guid_header = me->mw_act-guid_header
        IMPORTING
          ew_log_handle  = lw_log_handle.

      "Add BAL LOG - Msg
      CALL METHOD lo_bal_log->bal_log_msg_add
        EXPORTING
          iw_log_handle = lw_log_handle.

      "Save BAL LOG
      CALL METHOD lo_bal_log->bal_db_save
        EXPORTING
          iw_log_handle = lw_log_handle.

      "Set Status = XXX to exit update (only insert ballog error)
      me->mw_sta-stepstatus = /tcsr/c_constants=>mc_status-reverse_error. "XXX

    ELSE.

      LOOP AT me->mt_po INTO me->mw_po WHERE vgabe = '1'.

        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
          EXPORTING
            materialdocument = me->mw_po-doc_number
            matdocumentyear  = me->mw_po-gjahr
          IMPORTING
            goodsmvt_headret = lw_head_ret
          TABLES
            return           = lt_return.

        IF lw_head_ret-mat_doc IS NOT INITIAL.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
          <fs_po_del>-guid_header = me->mv_guid_header.
          <fs_po_del>-ebeln       = me->mw_po-ebeln.
          <fs_po_del>-ebelp       = me->mw_po-ebelp.
          <fs_po_del>-vgabe       = '1'.
          <fs_po_del>-doc_number  = me->mw_po-doc_number.
          <fs_po_del>-gjahr       = me->mw_po-gjahr.


          MESSAGE s362(/tcsr/msg) "Documento de Material & & estornado pelo Documento & &
             WITH me->mw_po-doc_number
                  me->mw_po-gjahr
                  lw_head_ret-mat_doc
                  lw_head_ret-doc_year
             INTO me->mv_dummy.

          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-reversedoc ).

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          "Create BAL LOG
          CREATE OBJECT lo_bal_log.
          CALL METHOD lo_bal_log->bal_log_create
            EXPORTING
              iv_guid_header = me->mw_act-guid_header
            IMPORTING
              ew_log_handle  = lw_log_handle.

          DELETE lt_return WHERE type NE 'E'.

          READ TABLE lt_return TRANSPORTING NO FIELDS INDEX 1.
          IF sy-subrc EQ 0.

            LOOP AT lt_return ASSIGNING <fs_return>.

              MESSAGE ID <fs_return>-id
                 TYPE <fs_return>-type
               NUMBER <fs_return>-number
                 WITH <fs_return>-message_v1
                      <fs_return>-message_v2
                      <fs_return>-message_v3
                      <fs_return>-message_v4 INTO me->mv_dummy.

              "Add BAL LOG - Msg
              CALL METHOD lo_bal_log->bal_log_msg_add
                EXPORTING
                  iw_log_handle = lw_log_handle.

            ENDLOOP.

          ELSE.

            MESSAGE e279(m8) INTO me->mv_dummy.   "Ocorreu um erro ao estornar
            "Add BAL LOG - Msg
            CALL METHOD lo_bal_log->bal_log_msg_add
              EXPORTING
                iw_log_handle = lw_log_handle.

          ENDIF.

          CALL METHOD lo_bal_log->bal_db_save
            EXPORTING
              iw_log_handle = lw_log_handle.

          "Set Status = XXX to exit update (only insert ballog error)
          me->mw_sta-stepstatus = /tcsr/c_constants=>mc_status-reverse_error. "XXX

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_MIGO_SAVE.

    DATA:
      lw_po_list_unass TYPE /tcsr/s_po_list,
      lw_po_list_assig TYPE /tcsr/s_po_list.
    DATA:
      lv_mblnr TYPE mblnr,
      lv_mjahr TYPE mjahr.
    FIELD-SYMBOLS:
      <fs_po_list_unass> TYPE /tcsr/s_po_list,
      <fs_po_list_assig> TYPE /tcsr/s_po_list,
      <fs_po_upd>        TYPE /tcsr/t_po,
      <fs_po_del>        TYPE /tcsr/t_po.
    DATA:
      lo_param     TYPE REF TO /tcsr/c_param,
      lo_exception TYPE REF TO /tcsr/cx_exception.

*------------------------------------------------

    CREATE OBJECT lo_param.

    "--------------------------------------------
    " CHECK: IF MIGO was UNASSIGNED
    "--------------------------------------------
    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass WHERE mblnr IS NOT INITIAL AND
                                                             mjahr IS NOT INITIAL.

      "Check MIGO is assigned
      READ TABLE me->mt_po_list_assig INTO lw_po_list_assig WITH KEY mblnr = lw_po_list_unass-mblnr
                                                                     mjahr = lw_po_list_unass-mjahr.
      CHECK: sy-subrc NE 0.

      MESSAGE s359(/tcsr/msg) INTO me->mv_dummy "Desassociado Documento de Material & &
                              WITH lw_po_list_unass-mblnr.
      me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-unassigned ).

      "Append PO/ITEM UNASSIGNED
      APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
      <fs_po_del>-guid_header = me->mv_guid_header.
      <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
      <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
      <fs_po_del>-vgabe       = '1'.
      <fs_po_del>-doc_number  = lw_po_list_unass-mblnr.
      <fs_po_del>-gjahr       = lw_po_list_unass-mjahr.

      CLEAR: me->mw_act-tolerance.

    ENDLOOP.

    "--------------------------------------------
    " CHECK: IF PO was UNASSIGNED
    "--------------------------------------------
    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass WHERE mblnr IS INITIAL AND
                                                             ebeln IS NOT INITIAL.

      "Check PO is assigned
      READ TABLE me->mt_po_list_assig INTO lw_po_list_assig WITH KEY ebeln = lw_po_list_unass-ebeln
                                                                     ebelp = lw_po_list_unass-ebelp.
      IF sy-subrc NE 0.

        MESSAGE s311(/tcsr/msg) INTO me->mv_dummy "Desassociado Pedido de Compra & e item &
                                WITH lw_po_list_unass-ebeln lw_po_list_unass-ebelp.
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-unassigned ).

        "Append PO/ITEM UNASSIGNED
        APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
        <fs_po_del>-guid_header = me->mv_guid_header.
        <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
        <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.

        "MIGO was assigned, remove register with PO
      ELSEIF sy-subrc = 0 AND lw_po_list_assig-mblnr IS NOT INITIAL.
        APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
        <fs_po_del>-guid_header = me->mv_guid_header.
        <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
        <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
      ENDIF.

      CLEAR: me->mw_act-tolerance.

    ENDLOOP.


    "--------------------------------------------
    " CHECK if PO or PO/MIGO - was set from Monitor
    "--------------------------------------------
    LOOP AT me->mt_po_list_assig INTO me->mw_po_list_assig.

      "Check if CREATE MIGO
      IF me->mw_po_list_assig-mblnr IS NOT INITIAL.

        "----------------------------------------
        "   ASSIGNED MIGO
        "----------------------------------------
        IF me->mv_action = /tcsr/c_constants=>mc_action-save.
          APPEND INITIAL LINE TO me->mt_po_upd ASSIGNING <fs_po_upd>.
          <fs_po_upd>-guid_header = me->mv_guid_header.
          <fs_po_upd>-ebeln       = me->mw_po_list_assig-ebeln.
          <fs_po_upd>-ebelp       = me->mw_po_list_assig-ebelp.
          <fs_po_upd>-vgabe       = '1'.
          <fs_po_upd>-doc_number  = me->mw_po_list_assig-mblnr.
          <fs_po_upd>-gjahr       = me->mw_po_list_assig-mjahr.

          "Update Active Table
          me->mw_act-tolerance      = me->mv_tolerance.       "Save Tolerance Value
          me->mw_act-tolerance_tax  = me->mv_tolerance_tax.   "Save Tolerance Tax

          MESSAGE s356(/tcsc/msg) INTO me->mv_dummy "Associado Documento de material & &
                                  WITH lw_po_list_assig-mblnr lw_po_list_assig-mjahr.
          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-success ).

          "Execute prerequisites for MIGO
          me->mt_po[] = me->mt_po_upd[].
        ENDIF.
        me->execute_prereq_migo( ).

      ELSE.
        "----------------------------------------
        "   ASSIGNED PO
        "----------------------------------------
        APPEND INITIAL LINE TO me->mt_po_upd ASSIGNING <fs_po_upd>.
        <fs_po_upd>-guid_header = me->mv_guid_header.
        <fs_po_upd>-ebeln       = me->mw_po_list_assig-ebeln.
        <fs_po_upd>-ebelp       = me->mw_po_list_assig-ebelp.
        <fs_po_upd>-menge       = me->mw_po_list_assig-menge_iv.
        <fs_po_upd>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.

        IF me->mv_action = /tcsr/c_constants=>mc_action-save.
          "Update Active Table
          me->mw_act-tolerance      = me->mv_tolerance.       "Save Tolerance Value
          me->mw_act-tolerance_tax  = me->mv_tolerance_tax.   "Save Tolerance Tax

          MESSAGE s312(/tcsr/msg) INTO me->mv_dummy       "Associado Pedido de Compras & e item
                                  WITH me->mw_po_list_assig-ebeln me->mw_po_list_assig-ebelp.
          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-pending ).
        ENDIF.

        "----------------------------------------
        "   CREATE MIGO
        "----------------------------------------
        IF  me->mv_not_create IS INITIAL.    "Create MIGO only if user has accepted
          "Or running automatically

          TRY.
              "Execute prerequisites for MIGO
              me->execute_prereq_migo( EXPORTING iv_prereq_value = abap_true ).

            CATCH /tcsr/cx_exception INTO lo_exception.
              "Returned Error
              MESSAGE ID lo_exception->if_t100_message~t100key-msgid
                 TYPE 'E'
               NUMBER lo_exception->if_t100_message~t100key-msgno
                 WITH lo_exception->if_t100_message~t100key-attr1
                      lo_exception->if_t100_message~t100key-attr2
                      lo_exception->if_t100_message~t100key-attr3
                      lo_exception->if_t100_message~t100key-attr4
                 INTO me->mv_dummy.
              me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
              EXIT.
          ENDTRY.

*          me->execute_migo_create( EXPORTING iv_ebeln = me->mw_po_list_assig-ebeln
*                                             iv_ebelp = me->mw_po_list_assig-ebelp
*                                    IMPORTING ev_mblnr = lv_mblnr
*                                              ev_mjahr = lv_mjahr ).

          IF lv_mblnr IS NOT INITIAL.

            "Update Material Document created
            <fs_po_upd>-vgabe      = '1'.
            <fs_po_upd>-doc_number = lv_mblnr.
            <fs_po_upd>-gjahr      = lv_mjahr.


            "Delete entry without material document
            APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
            <fs_po_del>-guid_header = me->mv_guid_header.
            <fs_po_del>-ebeln       = me->mw_po_list_assig-ebeln.
            <fs_po_del>-ebelp       = me->mw_po_list_assig-ebelp.


            "Execute prerequisites for MIGO
            me->mt_po[] = me->mt_po_upd[].
            me->execute_prereq_migo(
                                EXPORTING
                                   iv_no_success_msg = abap_true ).

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


METHOD EXECUTE_MIRO_CREATE.

  TYPES: BEGIN OF ty_lfa1,
           lifnr TYPE lfa1-lifnr,
           txjcd TYPE lfa1-txjcd,
           stcd1 TYPE lfa1-stcd1,
           stcd2 TYPE lfa1-stcd2,
           bukrs TYPE lfb1-bukrs,
           zwels TYPE lfb1-zwels,
         END OF ty_lfa1.
  TYPES: BEGIN OF ty_ekpa,
           ebeln TYPE ekpa-ebeln,
           ekorg TYPE ekpa-ekorg,
           parvw TYPE ekpa-parvw,
           lifn2 TYPE ekpa-lifn2,
           stcd2 TYPE lfa1-stcd2,
           stcd1 TYPE lfa1-stcd1,
         END OF ty_ekpa.
  TYPES: BEGIN OF ty_ekpo,
           ebeln TYPE ekpo-ebeln,
           ebelp TYPE ekpo-ebelp,
           mwskz TYPE ekpo-mwskz,
         END OF ty_ekpo.
  TYPES: BEGIN OF ty_ekkn.
          INCLUDE TYPE ekkn.
  TYPES: bukrs TYPE ekpo-bukrs,
         END OF ty_ekkn.
  TYPES: BEGIN OF ty_mseg,
           mblnr TYPE mseg-mblnr,
           mjahr TYPE mseg-mjahr,
           zeile TYPE mseg-zeile,
           menge TYPE mseg-menge,
           meins TYPE mseg-meins,
           erfmg TYPE mseg-erfmg, "Important for process PO-MIGO-MIRO
           erfme TYPE mseg-erfme, "Important for process PO-MIGO-MIRO
           dmbtr TYPE mseg-dmbtr,
           ebeln TYPE mseg-ebeln,
           ebelp TYPE mseg-ebelp,
           lfbja TYPE mseg-lfbja,
           lfbnr TYPE mseg-lfbnr,
           lfpos TYPE mseg-lfpos,
           zekkn TYPE mseg-zekkn,
           sakto TYPE mseg-sakto,
           gsber TYPE mseg-gsber,
           kostl TYPE mseg-kostl,
           kokrs TYPE mseg-kokrs,
           prctr TYPE mseg-prctr,
           aufnr TYPE mseg-aufnr,
         END OF ty_mseg.
  TYPES: BEGIN OF ty_ekbe,
           ebeln TYPE ekbe-ebeln,
           ebelp TYPE ekbe-ebelp,
           zekkn TYPE ekbe-zekkn,
           vgabe TYPE ekbe-vgabe,
           gjahr TYPE ekbe-gjahr,
           belnr TYPE ekbe-belnr,
           buzei TYPE ekbe-buzei,
           lfgja TYPE ekbe-lfgja,
           lfbnr TYPE ekbe-lfbnr,
           lfpos TYPE ekbe-lfpos,
         END OF ty_ekbe.
  TYPES: BEGIN OF ty_po_sum       ,
           ebeln TYPE ekpo-ebeln,
           ebelp TYPE ekpo-ebelp,
           dmbtr TYPE /tcsr/t_po-dmbtr,
         END OF ty_po_sum.
  DATA:
    lt_lfbw           TYPE TABLE OF lfbw,
    lt_ekko           TYPE TABLE OF ekko,
    lt_ekpo           TYPE TABLE OF ekpo,
    lt_ekkn           TYPE SORTED TABLE OF ty_ekkn WITH UNIQUE KEY ebeln ebelp zekkn,
    lt_eskn           TYPE TABLE OF eskn,
    lt_eskl           TYPE TABLE OF eskl,
    lt_eskl_lfpos     TYPE TABLE OF eskl,
    lt_esll           TYPE TABLE OF esll,
    lt_esll_pack      TYPE TABLE OF esll,
    lt_itemdata       TYPE TABLE OF bapi_incinv_create_item,
    lt_accountingdata TYPE TABLE OF bapi_incinv_create_account,
    lt_glacc          TYPE TABLE OF bapi_incinv_create_gl_account,
    lt_taxdata        TYPE TABLE OF bapi_incinv_create_tax,
    lt_withtaxdata    TYPE TABLE OF bapi_incinv_create_withtax,
    lt_bapi_return    TYPE TABLE OF bapiret2,
    lt_ekpa           TYPE TABLE OF ty_ekpa,
    lt_essr           TYPE TABLE OF essr,
    lt_lfa1           TYPE TABLE OF ty_lfa1,
    lt_eskn_aux       TYPE TABLE OF eskn,
    lt_eskl_aux       TYPE TABLE OF eskl,
    lt_esll_aux       TYPE TABLE OF esll,
    lt_mseg           TYPE SORTED TABLE OF ty_mseg WITH NON-UNIQUE KEY ebeln ebelp lfbja lfbnr lfpos,
    lt_ekbe           TYPE TABLE OF ty_ekbe,
    lt_t001b          TYPE TABLE OF t001b,
    lt_po_sum         TYPE TABLE OF ty_po_sum.

  DATA:
    lw_t001b          TYPE t001b,
    lw_lfa1           TYPE ty_lfa1,
    lw_lfbw           TYPE lfbw,
    lw_ekpa           TYPE ty_ekpa,
    lw_ekko           TYPE ekko,
    lw_ekpo           TYPE ekpo,
    lw_ekkn           TYPE ty_ekkn,
    lw_essr           TYPE essr,
    lw_esll           TYPE esll,
    lw_eskl           TYPE eskl,
    lw_headerdata     TYPE bapi_incinv_create_header,
    lw_itemdata       TYPE bapi_incinv_create_item,
    lw_accountingdata TYPE bapi_incinv_create_account,
    lw_glacc          TYPE bapi_incinv_create_gl_account,
    lw_taxdata        TYPE bapi_incinv_create_tax,
    lw_withtaxdata    TYPE bapi_incinv_create_withtax,
    lw_bapi_return    TYPE bapiret2,
    "BalLog
    lw_log_handle     TYPE balloghndl,
    lw_po             TYPE /tcsr/t_po,
    lw_proctyp        TYPE /tcsr/t_proctyp,
    lw_po_sum         TYPE ty_po_sum.
  DATA:
    lv_vlr_inss            TYPE string,
    lv_vlr_ir              TYPE string,
    lv_vlr_xml             TYPE string,
    lv_cat_ret_inss        TYPE string,
    lv_doc_type            TYPE bapi_incinv_create_header-doc_type,
    lv_parvw_vendor        TYPE wyt3-parvw,
    lv_ref_doc_no          TYPE xblnr_long,
    lv_doc_item            TYPE bapi_incinv_create_item-invoice_doc_item,
    lv_inss_vendor         TYPE /tcsr/e_inssvendor,
    lv_inss_vendor_xml     TYPE /tcsr/e_inssvendor,
    lv_inss_vendor_d       TYPE p DECIMALS 2 LENGTH 10,
    lv_inss_vendor_calc    TYPE p DECIMALS 2 LENGTH 10,
    lv_inssbase            TYPE p DECIMALS 2 LENGTH 10,
    lv_inssvalue           TYPE p DECIMALS 2 LENGTH 10,
    lv_firstday            TYPE sy-datum,
    lv_per_close           TYPE sy-datum,
    lv_strlen              TYPE i,
    lv_extra               TYPE i,
    lv_vlr_serv	           TYPE char15,
    lv_vlr_ded             TYPE char15,
    lv_aliq_inss_vendor	   TYPE char15,
    lv_inss_reduc_s        TYPE char45,
    lv_inss_reduc          TYPE c,
    lv_vlr_inss_d(10)      TYPE p DECIMALS 2,
    lv_vlr_ir_d(10)        TYPE p DECIMALS 2,
    lv_vlr_xml_d(10)       TYPE p DECIMALS 2,
    lv_inss_reduc_d(10)    TYPE p DECIMALS 2,
    lv_calc_inss(10)       TYPE p DECIMALS 2,
    lv_opt_simplesnacional TYPE char3,
    lv_vlr_serv_d(10)      TYPE p DECIMALS 2,
    lv_fill                TYPE abap_bool,
    lv_ebeln               TYPE ekpo-ebeln,
    lv_ebelp               TYPE ekpo-ebelp,
    lv_branch              TYPE t001w-j_1bbranch,
    lv_mcod1               TYPE lfa1-mcod1,
    lv_xblnr               TYPE num06,
    lv_posic               TYPE i,
    lv_name                TYPE lfa1-mcod1.
  DATA:
    lv_belnr      TYPE bapi_incinv_fld-inv_doc_no,
    lv_gjahr      TYPE bapi_incinv_fld-fisc_year,
    lv_tabix      TYPE sy-tabix,
    lv_tabix_esll TYPE sy-tabix,
    lv_verif_cod  TYPE j_1bnfdoc-checod.
  FIELD-SYMBOLS:
    <fs_eskn>           TYPE eskn,
    <fs_eskl>           TYPE eskl,
    <fs_eskl_lfpos>     TYPE eskl,
    <fs_esll>           TYPE esll,
    <fs_esll_pack>      TYPE esll,
    <fs_mseg>           TYPE ty_mseg,
    <fs_ekbe>           TYPE ty_ekbe,
    <fs_accountingdata> TYPE bapi_incinv_create_account.

  DATA:
    lr_mkoar      TYPE RANGE OF t001b-mkoar,
    lr_witht_inss TYPE RANGE OF witht,
    lr_witht_ir   TYPE RANGE OF witht.
  DATA:
    lo_param   TYPE REF TO /tcsr/c_param,
    lo_bal_log TYPE REF TO /tcsr/c_bal_log.
*-----------------------------------------------------------------------

*  "Get Doc Type and NF Type for the process
*  SELECT SINGLE *
*    INTO lw_proctyp
*    FROM /tcsr/t_proctyp
*    WHERE proctyp = me->mw_act-proctyp.
*  IF lw_proctyp-blart IS INITIAL.
*    MESSAGE e005(/tcsr/msg) "Parâmetro & não foi cadastrado
*       INTO me->mv_dummy WITH 'MIRO_DOCTYPE'.
*    me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
*    EXIT.
*  ELSE.
*    lv_doc_type = lw_proctyp-blart.
*  ENDIF.
*
*  IF me->mv_nftype IS INITIAL.
*    IF lw_proctyp-j_1bnftype IS INITIAL.
*      MESSAGE e005(/tcsr/msg) "Parâmetro & não foi cadastrado
*         INTO me->mv_dummy WITH 'MIRO_NFTYPE'.
*      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
*      EXIT.
*    ELSE.
*      me->mv_nftype = lw_proctyp-j_1bnftype.
*    ENDIF.
*  ENDIF.

  CREATE OBJECT: lo_param.

  "GET PARTNER FUNCTION(FO) FROM VENDOR
  lv_parvw_vendor = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                             iv_name     = 'PARVW_VENDOR' ).

  CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
    EXPORTING
      input  = lv_parvw_vendor
    IMPORTING
      output = lv_parvw_vendor.

  "Get INSS aliq to vendor
  lv_inss_vendor = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                            iv_name     = 'INSS_ALIQ' ).

  lv_inss_vendor_d = lv_inss_vendor.

  lv_opt_simplesnacional = me->mw_xml_rpsx-inf_rps-optante_simples_nacional.

  " Get Data to calculate INSS to vendor
  lv_vlr_serv         = me->mw_xml_rpsx-inf_rps-servicos-valores-valor_servicos.
  lv_vlr_ded          = me->mw_xml_rpsx-inf_rps-servicos-valores-valor_deducoes.

  lv_vlr_inss = me->mw_xml_rpsx-inf_rps-servicos-valores-valor_inss.
  TRANSLATE lv_vlr_inss USING ',.'.
  CONDENSE lv_vlr_inss NO-GAPS.
  lv_vlr_inss_d = lv_vlr_inss.

  lv_vlr_ir = me->mw_xml_rpsx-inf_rps-servicos-valores-valor_ir.
  TRANSLATE lv_vlr_ir USING ',.'.
  CONDENSE lv_vlr_ir NO-GAPS.
  lv_vlr_ir_d = lv_vlr_ir.

  IF lv_vlr_inss_d IS NOT INITIAL.

    CREATE OBJECT: lo_param.

    lv_vlr_xml = me->mw_xml_rpsx-inf_rps-servicos-valores-valor_servicos.
    TRANSLATE lv_vlr_xml USING ',.'.
    CONDENSE lv_vlr_xml NO-GAPS.
    lv_vlr_xml_d = lv_vlr_xml.

    "GET BASE REDUC
    lv_inss_reduc_s = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                   iv_name = 'INSS_BASE_REDUC' ).
    TRANSLATE lv_inss_reduc_s USING ',.'.
    CONDENSE lv_inss_reduc_s NO-GAPS.
    lv_inss_reduc_d = lv_inss_reduc_s.

    lv_calc_inss = lv_vlr_xml_d * ( lv_inss_reduc_d / 100 ).

    IF lv_calc_inss NE lv_vlr_inss_d.

      lv_inss_reduc = abap_true.

    ENDIF.
  ENDIF.

  lo_param->get_selection( EXPORTING iv_programm = 'PROCESS_STEPS'
                                     iv_name     = 'CAT_RET_INSS'
                           CHANGING  ct_range    = lr_witht_inss ).


  "GET ACCOUNT TYPE - MKOAR FIELD
  CALL METHOD lo_param->get_selection
    EXPORTING
      iv_programm = 'PROCESS_STEPS'
      iv_name     = 'POSTING_PERIOD_MKOAR'
    CHANGING
      ct_range    = lr_mkoar[].

*-----------------------------------------------------------------------
  CHECK: me->mt_po[] IS NOT INITIAL.

  "Select PO Header
  SELECT *
    INTO TABLE lt_ekko
    FROM ekko
    FOR ALL ENTRIES IN me->mt_po
    WHERE ebeln = me->mt_po-ebeln.

  "Select PO Itens
  SELECT *
    FROM ekpo
    INTO TABLE lt_ekpo
    FOR ALL ENTRIES IN me->mt_po
    WHERE ebeln = me->mt_po-ebeln
      AND ebelp = me->mt_po-ebelp.
  IF sy-subrc NE 0.
    MESSAGE e404(/tcsr/msg) "Pedido item não existem no SAP
       INTO me->mv_dummy.
    me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).

    EXIT.
  ELSE.
    SORT lt_ekpo BY ebeln ebelp.
  ENDIF.

  LOOP AT me->mt_po INTO lw_po.
    READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_po-ebeln
                                             ebelp = lw_po-ebelp
                                                  BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e232(/tcsr/msg) "Pedido & item & não existe no SAP
         INTO me->mv_dummy WITH lw_po-ebeln lw_po-ebelp.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
    ENDIF.
  ENDLOOP.

*-----------------------------------------------------------------------

  "Select GL Acc Data
  SELECT *
    FROM ekkn
    INTO TABLE lt_ekkn
    FOR ALL ENTRIES IN lt_ekpo
    WHERE ebeln = lt_ekpo-ebeln
      AND ebelp = lt_ekpo-ebelp.

  LOOP AT lt_ekkn INTO lw_ekkn.
    lv_tabix = sy-tabix.
    READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_ekkn-ebeln
                                             ebelp = lw_ekkn-ebelp
                                                    BINARY SEARCH.
    IF sy-subrc = 0.
      lw_ekkn-bukrs = lw_ekpo-bukrs.
      MODIFY lt_ekkn FROM lw_ekkn INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

  IF lt_ekkn[] IS NOT INITIAL.

    SELECT * "rrcty, bukrs, mkoar, bkont, vkont, frye1, frpe1
      FROM t001b
      INTO TABLE lt_t001b
      FOR ALL ENTRIES IN lt_ekkn
      WHERE bukrs EQ lt_ekkn-bukrs
        AND mkoar IN lr_mkoar[]
        AND bkont GT lt_ekkn-sakto
        AND vkont LE lt_ekkn-sakto.

    LOOP AT lt_t001b INTO lw_t001b.
      IF lw_t001b-frye1 LT sy-datum(4) OR
     ( lw_t001b-frye1 EQ sy-datum(4) AND lw_t001b-frpe1+1(2) LE sy-datum+4(2) ).
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = sy-datum
        IMPORTING
          e_date = lv_firstday.
      ADD 1 TO lv_firstday.

      CALL FUNCTION 'BKK_GET_NEXT_WORKDAY'
        EXPORTING
          i_date         = lv_firstday
          i_calendar1    = 'BR'
        IMPORTING
          e_workday      = lv_per_close
        EXCEPTIONS
          calendar_error = 1
          OTHERS         = 2.
      EXIT.
    ENDLOOP.

  ENDIF.

  READ TABLE lt_ekko INTO lw_ekko INDEX 1.

  "Select Vendor Data
  SELECT SINGLE lfa1~lifnr lfa1~txjcd lfa1~stcd1 lfa1~stcd2 lfb1~bukrs lfb1~zwels
    INTO lw_lfa1
    FROM lfa1
    INNER JOIN lfb1
       ON lfb1~lifnr EQ lfa1~lifnr
    WHERE lfa1~lifnr = lw_ekko-lifnr
      AND lfb1~bukrs = lw_ekko-bukrs.


*---- CHECK if CNPF/CNPJ from Vendor is differente from Prestador
*---- select Partner Functions (FO)
  IF lw_lfa1-stcd2 NE me->mw_hd-p_cpf
  OR lw_lfa1-stcd1 NE me->mw_hd-p_cnpj.

    IF lv_parvw_vendor IS NOT INITIAL.
      "Select Partner function(EF) from Vendor
      SELECT ekpa~ebeln
             ekpa~ekorg
             ekpa~parvw
             ekpa~lifn2
             lfa1~stcd2
             lfa1~stcd1
        INTO TABLE lt_ekpa
        FROM ekpa
        INNER JOIN lfa1
           ON lfa1~lifnr = ekpa~lifn2
        INNER JOIN lfb1
           ON lfb1~lifnr = lfa1~lifnr
        FOR ALL ENTRIES IN lt_ekko
        WHERE ekpa~ebeln = lt_ekko-ebeln
          AND ekpa~ekorg = lt_ekko-ekorg
          AND ekpa~parvw = lv_parvw_vendor
          AND lfa1~sperr = ''
          AND lfb1~bukrs = lt_ekko-bukrs
          AND lfb1~sperr = ''.
    ENDIF.

    "Check if Partner Function was not found
    "or CNF/CNPJ of Part.func is different from Prestador
    IF lw_ekpa IS INITIAL OR ( lw_ekpa-stcd2 NE me->mw_hd-p_cpf AND
                               lw_ekpa-stcd1 NE me->mw_hd-p_cnpj ).

      IF me->mw_hd-p_cpf IS NOT INITIAL.
        MESSAGE e250(/tcsr/msg) "& & do Prestador diferente do Fornecedor e Função Parceiro do Pedido &
           INTO me->mv_dummy WITH 'CPF' me->mw_hd-p_cpf lw_ekko-ebeln.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
        EXIT.
      ELSE.
        MESSAGE e250(/tcsr/msg) "& & do Prestador diferente do Fornecedor e Função Parceiro do Pedido &
           INTO me->mv_dummy WITH 'CNPJ' me->mw_hd-p_cnpj lw_ekko-ebeln.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
        EXIT.
      ENDIF.

    ENDIF.
  ENDIF.

  CASE me->mw_act-proctyp.

*-----------------------------------------------------------------------
*   PO-MIRO
*-----------------------------------------------------------------------
    WHEN 'PROCIV'.

      "Get packno header
      SELECT *
        INTO TABLE lt_esll_pack
        FROM esll
        FOR ALL ENTRIES IN lt_ekpo
        WHERE packno = lt_ekpo-packno.
      IF sy-subrc = 0.

        "Get packno items
        SELECT *
          INTO TABLE lt_esll
          FROM esll
          FOR ALL ENTRIES IN lt_esll_pack
          WHERE packno = lt_esll_pack-sub_packno.

        "Get link between service line and account
        SELECT *
          INTO TABLE lt_eskl
          FROM eskl
          FOR ALL ENTRIES IN lt_esll
          WHERE packno = lt_esll-packno AND
                introw = lt_esll-introw.

        SORT lt_esll_pack BY packno.
        SORT lt_esll BY packno extrow.
        SORT lt_eskl BY packno introw.
      ENDIF.

      LOOP AT me->mt_po INTO lw_po.
        lw_po_sum-ebeln = lw_po-ebeln.
        lw_po_sum-ebelp = lw_po-ebelp.
        IF lw_po-dmbtr = 1. "Inverted
          lw_po_sum-dmbtr = lw_po-menge.
        ELSE.
          lw_po_sum-dmbtr = lw_po-dmbtr.
        ENDIF.
        COLLECT lw_po_sum INTO lt_po_sum.
      ENDLOOP.

      SORT lt_po_sum BY ebeln ebelp.
      SORT me->mt_po BY ebeln ebelp.

      LOOP AT me->mt_po INTO lw_po.

        READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_po-ebeln
                                                 ebelp = lw_po-ebelp
                                                      BINARY SEARCH.
        IF lv_ebeln NE lw_po-ebeln OR
           lv_ebelp NE lw_po-ebelp.

          lv_ebeln = lw_po-ebeln.
          lv_ebelp = lw_po-ebelp.
          lv_doc_item = lv_doc_item + 1.

          "-------------------------------------------------------------------
          " ITEMDATA
          "-------------------------------------------------------------------
          CLEAR: lw_itemdata.
          lw_itemdata-invoice_doc_item    = lv_doc_item.                  "Document Item in Invoice Document
          lw_itemdata-po_number           = lw_ekpo-ebeln.                "Purchase Order Number
          lw_itemdata-po_item             = lw_ekpo-ebelp.                "Item Number of Purchasing Document
          lw_itemdata-tax_code            = lw_ekpo-mwskz.                "Tax Code
          lw_itemdata-taxjurcode          = lw_lfa1-txjcd.               "Tax Jurisdiction

          READ TABLE lt_po_sum INTO lw_po_sum WITH KEY ebeln = lw_po-ebeln
                                                       ebelp = lw_po-ebelp
                                                       BINARY SEARCH.

          READ TABLE lt_esll_pack INTO lw_esll WITH KEY packno = lw_ekpo-packno.

          READ TABLE lt_esll INTO lw_esll WITH KEY packno = lw_esll-sub_packno.

          READ TABLE lt_eskl INTO lw_eskl WITH KEY packno = lw_esll-packno
                                                   introw = lw_esll-introw.

          "Amount in document currency
          IF lw_po_sum-dmbtr IS NOT INITIAL.
            lw_itemdata-item_amount = lw_po_sum-dmbtr.
          ELSE.
            lw_itemdata-item_amount = lw_eskl-netwr.
          ENDIF.

          APPEND lw_itemdata TO lt_itemdata.
        ENDIF.

        "-----------------------------------------------------------------
        " ACCOUNTINGDATA
        "-----------------------------------------------------------------
        READ TABLE lt_esll_pack ASSIGNING <fs_esll_pack> WITH KEY packno = lw_ekpo-packno
                                                                        BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE lt_esll ASSIGNING <fs_esll> WITH KEY packno = <fs_esll_pack>-sub_packno
                                                          extrow = lw_po-extrow
                                                                  BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE lt_eskl ASSIGNING <fs_eskl> WITH KEY packno = <fs_esll>-packno
                                                            introw = <fs_esll>-introw
                                                                    BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE lt_ekkn INTO lw_ekkn WITH KEY ebeln = lw_ekpo-ebeln
                                                       ebelp = lw_ekpo-ebelp
                                                       zekkn = <fs_eskl>-zekkn
                                                            BINARY SEARCH.
              IF sy-subrc = 0.

                "One service line can have more than one accounting. Sum values
                READ TABLE lt_accountingdata ASSIGNING <fs_accountingdata>
                                              WITH KEY invoice_doc_item = lv_doc_item
                                                       serial_no        = lw_ekkn-zekkn.
                IF sy-subrc = 0.
                  IF lw_po-dmbtr IS NOT INITIAL.
                    IF lw_po-dmbtr = 1. " Inverted
                      <fs_accountingdata>-item_amount = <fs_accountingdata>-item_amount + lw_po-menge.
                    ELSE.
                      <fs_accountingdata>-item_amount = <fs_accountingdata>-item_amount + lw_po-dmbtr.
                    ENDIF.
                  ELSE.
                    <fs_accountingdata>-item_amount = <fs_accountingdata>-item_amount + <fs_eskl>-netwr.
                  ENDIF.
                ELSE.
                  CLEAR: lw_accountingdata.
                  lw_accountingdata-invoice_doc_item  = lv_doc_item.        "Document Item in Invoice Document
                  lw_accountingdata-serial_no         = lw_ekkn-zekkn.      "Sequential Number of Account Assignment
                  lw_accountingdata-tax_code          = lw_ekpo-mwskz.      "Tax Code (from PO)
                  lw_accountingdata-taxjurcode        = lw_ekkn-txjcd.      "Tax Jurisdiction
                  lw_accountingdata-gl_account        = lw_ekkn-sakto.      "G/L Account Number
                  lw_accountingdata-func_area         = lw_ekkn-fkber.
                  lw_accountingdata-func_area_long    = lw_ekkn-fkber.
                  lw_accountingdata-bus_area          = lw_ekkn-gsber.        "Business Area
                  lw_accountingdata-costcenter        = lw_ekkn-kostl.        "Cost Center
                  lw_accountingdata-co_area           = lw_ekkn-kokrs.        "Controlling Area
                  lw_accountingdata-profit_ctr        = lw_ekkn-prctr.        "Profit Center
                  lw_accountingdata-wbs_elem          = lw_ekkn-ps_psp_pnr.   "WBS Element
                  lw_accountingdata-asset_no          = lw_ekkn-anln1.        "Main Asset Number
                  lw_accountingdata-sub_number        = lw_ekkn-anln2.        "Asset Subnumber
                  lw_accountingdata-orderid           = lw_ekkn-aufnr.        "Order Number
                  lw_accountingdata-cmmt_item_long    = lw_ekkn-sakto.        "G/L Account Number
                  "Amount in document currency
                  IF lw_po-dmbtr IS NOT INITIAL.
                    IF lw_po-dmbtr = 1. " Inverted
                      lw_accountingdata-item_amount = lw_po-menge.
                    ELSE.
                      lw_accountingdata-item_amount = lw_po-dmbtr.
                    ENDIF.
                  ELSE.
                    lw_accountingdata-item_amount       = <fs_eskl>-netwr.       "Amount in document currency
                  ENDIF.
                  APPEND lw_accountingdata TO lt_accountingdata.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.

      ENDLOOP.

*-----------------------------------------------------------------------
*   PO-MIGO-MIRO
*-----------------------------------------------------------------------
    WHEN 'PROCGR'.

      SELECT mblnr
             mjahr
             zeile
             menge
             meins
             erfmg
             erfme
             dmbtr
             ebeln
             ebelp
             lfbja
             lfbnr
             lfpos
             zekkn
             sakto
             gsber
             kostl
             kokrs
             prctr
             aufnr
        FROM mseg
        INTO TABLE lt_mseg
        FOR ALL ENTRIES IN me->mt_po
        WHERE mblnr = me->mt_po-doc_number
          AND mjahr = me->mt_po-gjahr.

      SELECT ebeln
             ebelp
             zekkn
             vgabe
             gjahr
             belnr
             buzei
             lfgja
             lfbnr
             lfpos
        INTO TABLE lt_ekbe
        FROM ekbe
        FOR ALL ENTRIES IN me->mt_po
        WHERE gjahr = me->mt_po-gjahr
          AND belnr = me->mt_po-doc_number.
      IF sy-subrc = 0.
        SORT lt_ekbe BY gjahr belnr.
      ENDIF.

      LOOP AT lt_ekpo INTO lw_ekpo.

        lv_doc_item = lv_doc_item + 1.

*     "------------------------------------------
*     " ITEMDATA
*     "------------------------------------------
        LOOP AT lt_ekbe ASSIGNING <fs_ekbe> WHERE ebeln = lw_ekpo-ebeln AND
                                                  ebelp = lw_ekpo-ebelp.

          READ TABLE lt_mseg ASSIGNING <fs_mseg> WITH TABLE KEY ebeln = <fs_ekbe>-ebeln
                                                                ebelp = <fs_ekbe>-ebelp
                                                                lfbja = <fs_ekbe>-lfgja
                                                                lfbnr = <fs_ekbe>-lfbnr
                                                                lfpos = <fs_ekbe>-lfpos.
          CHECK: sy-subrc EQ 0.

          CLEAR: lw_itemdata.
          lw_itemdata-invoice_doc_item      = lv_doc_item.          "Document Item in Invoice Document
          lw_itemdata-po_number             = <fs_ekbe>-ebeln.      "Purchase Order Number
          lw_itemdata-po_item               = <fs_ekbe>-ebelp.      "Item Number of Purchasing Document

          lw_itemdata-ref_doc               = <fs_ekbe>-belnr.      "Document No. of a Reference Document
          lw_itemdata-ref_doc_year          = <fs_ekbe>-gjahr.      "Fiscal Year of Current Period
          lw_itemdata-ref_doc_it            = <fs_ekbe>-buzei.      "Item

          lw_itemdata-tax_code              = lw_ekpo-mwskz.      "Tax Code
          lw_itemdata-taxjurcode            = lw_lfa1-txjcd.        "Tax Jurisdiction

          lw_itemdata-item_amount           = <fs_mseg>-dmbtr.      "Amount in document currency
          lw_itemdata-quantity              = <fs_mseg>-erfmg.      "Quantity
          lw_itemdata-po_unit               = <fs_mseg>-erfme.      "Purchase Order Unit of Measure
          lw_itemdata-grir_clear_srv        = 'X'.                  "Clearing Indicator for GR/IR

          APPEND lw_itemdata TO lt_itemdata.
        ENDLOOP.

*     "------------------------------------------
*     " ACCOUNTINGDATA
*     "------------------------------------------
        LOOP AT lt_mseg ASSIGNING <fs_mseg> WHERE ebeln = lw_ekpo-ebeln AND
                                                  ebelp = lw_ekpo-ebelp.
          CLEAR: lw_accountingdata.

          READ TABLE lt_ekkn INTO lw_ekkn WITH TABLE KEY ebeln = <fs_mseg>-ebeln
                                                         ebelp = <fs_mseg>-ebelp
                                                         zekkn = <fs_mseg>-zekkn.

          " If there is no separation of items, ZEKKN is '00' in table MSEG. However, ZEKKN would be '01' in table EKKN.
          IF sy-subrc <> 0.
            READ TABLE lt_ekkn INTO lw_ekkn WITH TABLE KEY ebeln = <fs_mseg>-ebeln
                                                           ebelp = <fs_mseg>-ebelp
                                                           zekkn = '01'.
          ENDIF.
          IF sy-subrc = 0.
            lw_accountingdata-invoice_doc_item  = lv_doc_item.      "Document Item in Invoice Document
            lw_accountingdata-tax_code          = lw_ekpo-mwskz.  "Tax Code
            lw_accountingdata-taxjurcode        = lw_ekpo-txjcd.  "Tax Jurisdiction

            lw_accountingdata-item_amount       = <fs_mseg>-dmbtr. "Amount in document currency SN flush

            CALL METHOD me->chk_qty_unit_fill
              EXPORTING
                iv_pstyp = lw_ekpo-pstyp
                iv_lebre = lw_ekpo-lebre
                iv_vrtkz = lw_ekpo-vrtkz
              RECEIVING
                re_fill  = lv_fill.

            IF lv_fill EQ abap_true.
              lw_accountingdata-quantity        = <fs_mseg>-erfmg.  "Quantity
              lw_accountingdata-po_unit         = <fs_mseg>-erfme.  "Purchase Order Unit of Measure
              lw_accountingdata-po_unit_iso     = <fs_mseg>-erfme.  "Order unit in ISO code
            ENDIF.


            lw_accountingdata-serial_no       = lw_ekkn-zekkn.      "Sequential Number of Account Assignment
            lw_accountingdata-gl_account      = lw_ekkn-sakto.      "G/L Account Number
            lw_accountingdata-bus_area        = lw_ekkn-gsber.      "Business Area
            lw_accountingdata-costcenter      = lw_ekkn-kostl.      "Cost Center
            lw_accountingdata-co_area         = lw_ekkn-kokrs.      "Controlling Area
            lw_accountingdata-profit_ctr      = lw_ekkn-prctr.      "Profit Center
            lw_accountingdata-wbs_elem        = lw_ekkn-ps_psp_pnr. "WBS Element
            lw_accountingdata-func_area         = lw_ekkn-fkber.
            lw_accountingdata-func_area_long    = lw_ekkn-fkber.

            APPEND lw_accountingdata TO lt_accountingdata.
          ENDIF.
        ENDLOOP.

      ENDLOOP.
*-----------------------------------------------------------------------
*   PO-FRS-MIRO
*-----------------------------------------------------------------------
    WHEN 'PROCSES'.

      REFRESH: lt_eskn[],
               lt_eskl[],
               lt_eskl_lfpos[],
               lt_esll[],
               lt_esll_pack[],
               lt_essr[].

      "-----------------------------------------------------------------
      " GET DATA FROM SERV.ENTRYSHEET
      "-----------------------------------------------------------------
      LOOP AT me->mt_po INTO lw_po.
        REFRESH: lt_eskn_aux[], lt_eskl_aux[], lt_esll_aux[].


        CALL FUNCTION 'MS_READ_ENTRY_SHEET'
          EXPORTING
            i_lblni            = lw_po-doc_number
            i_with_accounts    = 'X'
            i_with_eskl        = 'X'
            i_with_esll        = 'X'
          IMPORTING
            e_essr             = lw_essr
          TABLES
            t_eskn             = lt_eskn_aux
            t_eskl             = lt_eskl_aux
            t_esll             = lt_esll_aux
          EXCEPTIONS
            sheet_not_found    = 1
            account_not_found  = 2
            sheet_not_buffered = 3
            OTHERS             = 4.
        IF sy-subrc = 0.
          APPEND lw_essr TO lt_essr.
          APPEND LINES OF lt_eskn_aux TO lt_eskn.
          APPEND LINES OF lt_eskl_aux TO lt_eskl.
          APPEND LINES OF lt_esll_aux TO lt_esll.

        ENDIF.
      ENDLOOP.

      "Header
      SORT lt_essr BY lblni.
      DELETE ADJACENT DUPLICATES FROM lt_essr COMPARING lblni.

      "Services
      SORT lt_esll      BY packno introw extrow.
      DELETE ADJACENT DUPLICATES FROM lt_esll COMPARING packno introw extrow.
      lt_esll_pack[] = lt_esll[].
      DELETE lt_esll_pack WHERE package = ''.
      DELETE lt_esll      WHERE package = 'X'.
      SORT lt_esll_pack BY packno introw extrow.

      "Account
      SORT lt_eskl BY packno introw numkn.
      DELETE ADJACENT DUPLICATES FROM lt_eskl COMPARING packno introw numkn.

      lt_eskl_lfpos[] = lt_eskl[].
      SORT lt_eskl_lfpos BY packno introw lfpos.
      DELETE ADJACENT DUPLICATES FROM lt_eskl_lfpos COMPARING packno introw lfpos.

      SORT lt_eskn BY packno zekkn.
      DELETE ADJACENT DUPLICATES FROM lt_eskn COMPARING packno zekkn.

      "---------------------------------------------
      " ESLL pack
      "---------------------------------------------
      LOOP AT lt_essr INTO lw_essr.

        READ TABLE lt_esll_pack ASSIGNING <fs_esll_pack>
                                WITH KEY packno = lw_essr-packno
                                BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        READ TABLE lt_esll ASSIGNING <fs_esll>
                           WITH KEY packno = <fs_esll_pack>-sub_packno
                           BINARY SEARCH.
        CHECK: sy-subrc EQ 0.


        "-----------------------------------------------------------------
        " Processo  - Rateio Quantidade ou Porcentagem
        "-----------------------------------------------------------------
        LOOP AT lt_esll ASSIGNING <fs_esll> FROM sy-tabix.
          IF <fs_esll>-packno NE <fs_esll_pack>-sub_packno.
            EXIT.
          ENDIF.

          LOOP AT lt_eskl_lfpos ASSIGNING <fs_eskl_lfpos>
                                WHERE packno = <fs_esll>-packno AND
                                      introw = <fs_esll>-introw.

            READ TABLE lt_eskl TRANSPORTING NO FIELDS
                               WITH KEY packno = <fs_eskl_lfpos>-packno
                                        introw = <fs_eskl_lfpos>-introw
                                        lfpos  = <fs_eskl_lfpos>-lfpos
                                        BINARY SEARCH.
            CHECK sy-subrc EQ 0.

            CLEAR: lw_itemdata.

            LOOP AT lt_eskl ASSIGNING <fs_eskl> FROM sy-tabix.
              IF <fs_eskl>-packno NE <fs_eskl_lfpos>-packno OR
                 <fs_eskl>-introw NE <fs_eskl_lfpos>-introw OR
                 <fs_eskl>-lfpos  NE <fs_eskl_lfpos>-lfpos.
                EXIT.
              ENDIF.

              CHECK: <fs_eskl>-loekz IS INITIAL.

              "Cenário com rateio de valor "<fs_esll>-vrtkz = 3", onde netwrt = 0,00
              CHECK: <fs_eskl>-netwr IS NOT INITIAL.

              "-----------------------------------------------------------------
              " ITEMDATA
              "-----------------------------------------------------------------
              IF lw_itemdata IS INITIAL.
                lv_doc_item = lv_doc_item + 1.
              ENDIF.
              lw_itemdata-invoice_doc_item      = lv_doc_item.          "Document Item in Invoice Document
              lw_itemdata-po_number             = lw_essr-ebeln.        "Purchase Order Number
              lw_itemdata-po_item               = lw_essr-ebelp.        "Item Number of Purchasing Document
              lw_itemdata-ref_doc               = lw_essr-lblni.        "Document No. of a Reference Document
              lw_itemdata-ref_doc_year          = lw_essr-erdat(4).     "Fiscal Year of Current Period
              lw_itemdata-ref_doc_it            = <fs_eskl>-lfpos.      "Item of a Reference Document
              lw_itemdata-tax_code              = <fs_esll>-mwskz.        "Tax Code
              lw_itemdata-taxjurcode            = <fs_esll>-txjcd.        "Tax Jurisdiction
              lw_itemdata-item_amount           = lw_itemdata-item_amount + <fs_eskl>-netwr.      "Amount in document currency "-02.06.2019
              lw_itemdata-quantity              = lw_itemdata-quantity + <fs_eskl>-menge.      "Quantity
              lw_itemdata-po_unit               = <fs_esll>-meins.      "Purchase Order Unit of Measure
              lw_itemdata-po_unit_iso           = <fs_esll>-meins.      "Order unit in ISO code
              lw_itemdata-po_pr_qnt             = <fs_eskl>-menge.      "Quantity in Purchase Order Price Unit
              lw_itemdata-po_pr_uom             = <fs_esll>-meins.      "Order Price Unit (Purchasing)
              lw_itemdata-po_pr_uom_iso         = <fs_esll>-meins.      "Purchase order price unit in ISO code
              lw_itemdata-sheet_no              = lw_essr-lblni.        "Entry Sheet Number
              lw_itemdata-sheet_item            = <fs_esll>-extrow.     "Line Number

              "-----------------------------------------------------------------
              " ACCOUNTINGDATA
              "-----------------------------------------------------------------
              LOOP AT lt_eskn ASSIGNING <fs_eskn> WHERE packno = lw_essr-lblni AND
                                                        zekkn = <fs_eskl>-zekkn.
                CLEAR: lw_accountingdata.
                lw_accountingdata-invoice_doc_item    = lv_doc_item.      "Document Item in Invoice Document
                lw_accountingdata-serial_no           = <fs_eskl>-zekkn.  "Sequential Number of Account Assignment

                lw_accountingdata-tax_code            = <fs_eskn>-mwskz.          "Tax Code (from PO)
                lw_accountingdata-taxjurcode          = <fs_eskn>-txjcd.        "Tax Jurisdiction
                lw_accountingdata-gl_account          = <fs_eskn>-sakto.        "G/L Account Number
                lw_accountingdata-func_area           = <fs_eskn>-fkber.        "Func.Area
                lw_accountingdata-func_area_long      = <fs_eskn>-fkber.        "Func.Area
                lw_accountingdata-bus_area            = <fs_eskn>-gsber.        "Business Area
                lw_accountingdata-costcenter          = <fs_eskn>-kostl.        "Cost Center
                lw_accountingdata-co_area             = <fs_eskn>-kokrs.        "Controlling Area
                lw_accountingdata-profit_ctr          = <fs_eskn>-prctr.        "Profit Center
                lw_accountingdata-wbs_elem            = <fs_eskn>-ps_psp_pnr.   "WBS Element
                lw_accountingdata-asset_no            = <fs_eskn>-anln1.        "Main Asset Number
                lw_accountingdata-sub_number          = <fs_eskn>-anln2.        "Asset Subnumber
                lw_accountingdata-orderid             = <fs_eskn>-aufnr.        "Order Number
*                lw_accountingdata-cmmt_item_long      = <fs_eskn>-sakto.        "G/L Account Number
*                lw_accountingdata-cmmt_item           = <fs_eskn>-sakto.        "G/L Account Number
*                lw_accountingdata-funds_ctr           = <fs_eskn>-kostl.        "Cost Center
                lw_accountingdata-budget_period       = <fs_eskn>-budget_pd.    "Budged Pariod
                lw_accountingdata-item_amount         = <fs_eskl>-netwr.        "Amount in document currency

                IF <fs_esll>-vrtkz NE '3'.
                  lw_accountingdata-quantity            = <fs_eskl>-menge.        "Quantity
                  lw_accountingdata-po_pr_qnt           = <fs_eskl>-menge.        "Quantity
                  lw_accountingdata-po_unit             = <fs_esll>-meins.        "Purchase Order Unit of Measure
                  lw_accountingdata-po_unit_iso         = <fs_esll>-meins.        "Order unit in ISO code
                  lw_accountingdata-po_pr_uom           = <fs_esll>-meins.        "Order unit in ISO code
                  lw_accountingdata-po_pr_uom_iso       = <fs_esll>-meins.        "Order unit in ISO code
                ENDIF.

                APPEND lw_accountingdata TO lt_accountingdata.

              ENDLOOP.  "LOOP AT lt_eskn
            ENDLOOP.  "LOOP AT lt_eskn
            APPEND lw_itemdata TO lt_itemdata.

          ENDLOOP.  "LOOP AT lt_eskl_lfpos

        ENDLOOP.  "LOOP AT lt_esll
      ENDLOOP.  "LOOP AT lt_essr
    WHEN OTHERS.
  ENDCASE.

  SORT lt_itemdata       BY invoice_doc_item.           "+03.10.2019
  SORT lt_accountingdata BY invoice_doc_item serial_no. "+03.10.2019

*-----------------------------------------------------------------------
*   WITHTAXDATA
*-----------------------------------------------------------------------
  READ TABLE lt_ekko INTO lw_ekko INDEX 1.

  SELECT *
    INTO TABLE lt_lfbw
    FROM lfbw
    WHERE lifnr     = lw_ekko-lifnr
      AND bukrs     = lw_ekko-bukrs
      AND wt_subjct = 'X'.
  IF sy-subrc EQ 0.
    LOOP AT lt_lfbw INTO lw_lfbw.
      lw_withtaxdata-split_key   = '000001'.
      lw_withtaxdata-wi_tax_type = lw_lfbw-witht.
      lw_withtaxdata-wi_tax_code = lw_lfbw-wt_withcd.

      "INSS
      IF lw_lfbw-witht IN lr_witht_inss AND lr_witht_inss[] IS NOT INITIAL. " AND
*         lv_vlr_inss_d IS NOT INITIAL.
        IF me->mw_act-inss_base IS NOT INITIAL.
          lw_withtaxdata-wi_tax_base = me->mw_act-inss_base.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      "IR
      IF lw_lfbw-witht IN lr_witht_ir AND lr_witht_ir[] IS NOT INITIAL. " AND
*         lv_vlr_ir_d IS NOT INITIAL.
        IF me->mw_act-ir_base IS NOT INITIAL.
          lw_withtaxdata-wi_tax_base = me->mw_act-ir_base.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND lw_withtaxdata TO lt_withtaxdata.
      FREE lw_withtaxdata.
    ENDLOOP.
  ENDIF.

*-----------------------------------------------------------------------
*   HEADERDATA
*-----------------------------------------------------------------------
  lw_headerdata-invoice_ind       = abap_true.              "Indicator: post invoice
  lw_headerdata-doc_type          = lv_doc_type.            "Document Type (RE)
  lw_headerdata-doc_date          = me->mw_hd-dtemissao.    "Document Date in Document
  IF lv_per_close IS INITIAL.
    lw_headerdata-pstng_date      = sy-datlo.               "Posting Date in the Document
  ELSE.
    lw_headerdata-pstng_date      = lv_per_close.           "Posting Date in the Document
  ENDIF.
  lw_headerdata-comp_code         = lw_ekko-bukrs.          "Company Code

  "Check if Partner Function(EF) was found in select above
  IF lw_ekpa-lifn2 IS NOT INITIAL.
    lw_headerdata-diff_inv        = lw_ekpa-lifn2.          "Different Invoicing Party
  ELSE.
    lw_headerdata-diff_inv        = lw_ekko-lifnr.          "Different Invoicing Party
  ENDIF.

  lw_headerdata-currency          = lw_ekko-waers.          "Currency Key
  lw_headerdata-currency_iso      = lw_ekko-waers.          "ISO currency code
  lw_headerdata-exch_rate         = '1.00000'.              "Direct Quoted Exchange Rate
  lw_headerdata-exch_rate_v       = '0.00000'.              "Indirect Quoted Exchange Rate
  lw_headerdata-gross_amount      = me->mw_hd-nfse_value.   "Gross Invoice Amount in Document Currency
  lw_headerdata-calc_tax_ind      = 'X'.                    "Calculate tax automatically
  lw_headerdata-pmnttrms          = lw_ekko-zterm.          "Terms of Payment Key (from parametre)
  lw_headerdata-bline_date        = me->mw_hd-dtemissao.    "Baseline Date for Due Date Calculation

*----- Check Payment Method
  IF lw_lfa1-zwels CO 'E '.
    lw_headerdata-pymt_meth       = 'E'.                    "Payment Method
  ENDIF.

*---- Create Ref.Doc.No: NFS-E
  lv_ref_doc_no = me->mw_hd-nfse_numero.
  SHIFT lv_ref_doc_no LEFT DELETING LEADING '0'.
  lv_strlen = strlen( lv_ref_doc_no ).
  IF lv_strlen > 6.
    lv_extra = lv_strlen - 6.
    lv_ref_doc_no = lv_ref_doc_no+lv_extra(6).
  ENDIF.
  UNPACK lv_ref_doc_no TO lv_xblnr.

*  "Serie
*  IF me->mw_hd-nfse_serie IS NOT INITIAL.
*    CONCATENATE lv_ref_doc_no '-' me->mw_hd-nfse_serie INTO lv_ref_doc_no.
*  ELSE.
*    CONCATENATE lv_ref_doc_no '-1' INTO lv_ref_doc_no.
*  ENDIF.

*  lw_headerdata-ref_doc_no_long   = lv_ref_doc_no.          "Reference Document Number
  lw_headerdata-j_1bnftype        = me->mv_nftype.          "Nota Fiscal Type (SET FROM POPUP MONITOR)
  lw_headerdata-ref_doc_no_long   = lv_xblnr.          "Reference Document Number
  lw_headerdata-ref_doc_no        = lv_xblnr.          "Reference Document Number
*  lw_headerdata-ref_doc_no        = lv_ref_doc_no.          "Reference Document Number

  lw_headerdata-pmnt_block        = me->mv_zlspr.
  lw_headerdata-bus_area          = lw_ekkn-gsber.        "Business area
  lw_headerdata-alloc_nmbr        = lv_xblnr.

  SELECT SINGLE mcod1
    INTO lv_mcod1
    FROM lfa1
    WHERE lifnr = lw_headerdata-diff_inv.

  lv_strlen = strlen( lv_mcod1 ).
  DO lv_strlen TIMES.
    lv_posic = lv_posic + 1.
    IF lv_mcod1+lv_posic(1) EQ space.
      lv_name = lv_mcod1(lv_posic).
      EXIT.
    ENDIF.
  ENDDO.

  CONCATENATE lv_name 'NF' lv_xblnr INTO lw_headerdata-item_text SEPARATED BY space.

  READ TABLE lt_ekpo INTO lw_ekpo INDEX 1.

  "Get branch
  SELECT SINGLE j_1bbranch
    INTO lv_branch
    FROM t001w
    WHERE werks = lw_ekpo-werks.

  IF lw_ekpo-knttp = 'F' OR lw_ekpo-knttp = 'P'.
    "Get Approver Code
    SELECT SINGLE cod_approver
      INTO lw_headerdata-header_txt
      FROM /tcsr/t_approve
      WHERE bukrs  = lw_ekko-bukrs
        AND branch = lv_branch.
  ENDIF.

  "---------------------------------------------------------------------
  " EXPORT MEMORY ID TO FILL J_1BNFDOC-PREFNO
  " BADI J_1BNF_ADD_DATA~ADD_DATA
  "---------------------------------------------------------------------
  DATA: lw_hd TYPE /tcsr/t_hd.
  lw_hd = me->mw_hd.
  EXPORT lw_hd = lw_hd TO MEMORY ID '/TCSR/T_HD'.

  lv_verif_cod = me->mw_xml_rpsx-inf_nfse-codigo_verificacao.
  EXPORT lv_verif_cod = lv_verif_cod TO MEMORY ID '/TCSR/CHECOD'.

  "---------------------------------------------------------------------
  " EXPORT MEMORY ID TO CHECK: Regra de substituícao
  " ZRFINRGGBS000 - FORM u231.
  "   IMPORT mr1m_ped   FROM MEMORY ID 'memo_ebeln'.
  "   IMPORT mr1m_pedit FROM MEMORY ID 'memo_ebelp'.
  "---------------------------------------------------------------------
  DATA: mr1m_ped   TYPE ekpo-ebeln.
  DATA: mr1m_pedit TYPE ekpo-ebelp.
  mr1m_ped = lw_ekko-ebeln.
  EXPORT mr1m_ped   FROM mr1m_ped   TO MEMORY ID 'memo_ebeln'.
  EXPORT mr1m_pedit FROM mr1m_pedit TO MEMORY ID 'memo_ebelp'.

  "Refresh Global datas from BAPI
  CALL METHOD me->call_refresh_bapis.

"*---> 01/07/2023 - Migração S4 - LO --> Material não foi utilizado
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'"#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata       = lw_headerdata
    IMPORTING
      invoicedocnumber = lv_belnr
      fiscalyear       = lv_gjahr
    TABLES
      itemdata         = lt_itemdata
      accountingdata   = lt_accountingdata
      glaccountdata    = lt_glacc
      taxdata          = lt_taxdata
      withtaxdata      = lt_withtaxdata
      return           = lt_bapi_return.

  IF lv_belnr IS NOT INITIAL.

    me->mw_act-belnr  = lv_belnr.
    me->mw_act-gjahr  = lv_gjahr.
    me->mw_act_upd = me->mw_act.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    MESSAGE s401(/tcsr/msg) "Revisão de fatura nº & & foi criado
       INTO me->mv_dummy WITH lv_belnr lv_gjahr.
    me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-invoiced ).
    EXIT.

  ELSE.

    DELETE lt_bapi_return WHERE type NE 'E'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    IF lt_bapi_return[] IS NOT INITIAL.

      "Create BAL LOG
      CREATE OBJECT lo_bal_log.
      CALL METHOD lo_bal_log->bal_log_create
        EXPORTING
          iv_guid_header = me->mv_guid_header
        IMPORTING
          ew_log_handle  = lw_log_handle.

      "Insert All Erro Messags
      LOOP AT lt_bapi_return INTO lw_bapi_return.
        "Set Error Messages
        MESSAGE ID lw_bapi_return-id
           TYPE lw_bapi_return-type
         NUMBER lw_bapi_return-number
           WITH lw_bapi_return-message_v1
                lw_bapi_return-message_v2
                lw_bapi_return-message_v3
                lw_bapi_return-message_v4
           INTO me->mv_dummy.
        "Add BAL LOG - Msg
        CALL METHOD lo_bal_log->bal_log_msg_add
          EXPORTING
            iw_log_handle = lw_log_handle.
      ENDLOOP.
      "Save BAL LOG
      CALL METHOD lo_bal_log->bal_db_save
        EXPORTING
          iw_log_handle = lw_log_handle.
    ENDIF.

    MESSAGE e403(/tcsr/msg) "Erro ao criar Revisão de Fatura
       INTO me->mv_dummy.
    me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
    EXIT.

  ENDIF.

ENDMETHOD.


  METHOD EXECUTE_ML81N_CONTINUE.

    DATA:
      lt_serv_line TYPE /tcsr/y_po_list,
      lt_po_list   TYPE /tcsr/y_po,
      lt_services  TYPE /tcsr/y_po.
    DATA:
      lw_po_list   TYPE /tcsr/t_po,
      lw_serv_line TYPE /tcsr/s_po_list,
      lw_services  TYPE /tcsr/t_po.
    DATA:
      lv_entrysheet      TYPE bapiessr-sheet_no,
      lv_acceptance      TYPE bapiessr-acceptance,
      lv_error           TYPE c,
      lv_create_frs_auto TYPE c.
    FIELD-SYMBOLS:
      <fs_po_del> TYPE /tcsr/t_po,
      <fs_po>     TYPE /tcsr/t_po.
    DATA:
      lo_exception TYPE REF TO /tcsr/cx_exception,
      lo_param     TYPE REF TO /tcsr/c_param.

    CREATE OBJECT lo_param.
    lv_create_frs_auto   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                    iv_name     = 'CREATE_FRS_AUTO' ).
    IF lv_create_frs_auto IS INITIAL.
      me->mv_not_create = abap_true.
    ENDIF.

    "Verify if at least PO was informed
    IF me->mt_po[] IS INITIAL.
      MESSAGE s317(/tcsr/msg) "No PO and/or Entry Sheet assigned
         INTO me->mv_dummy.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
      EXIT.
    ENDIF.

    lt_po_list[] = me->mt_po[].
    SORT lt_po_list BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM lt_po_list COMPARING ebeln ebelp.

    LOOP AT lt_po_list ASSIGNING <fs_po>.
      IF <fs_po>-doc_number IS INITIAL.

        "----------------------------------------
        "   CREATE SES(ML81N)
        "----------------------------------------
        IF  me->mv_not_create IS INITIAL.    "Create FRS only if user has accepted
          "Or running automatically
          CLEAR lv_error.
          TRY.
              "Execute prerequisites for MIGO
              me->execute_prereq_ml81n( EXPORTING iv_prereq_value = abap_true
                                        IMPORTING ev_prereq_error = lv_error ).

            CATCH /tcsr/cx_exception INTO lo_exception.
              "Returned Error
              MESSAGE ID lo_exception->if_t100_message~t100key-msgid
                 TYPE 'E'
               NUMBER lo_exception->if_t100_message~t100key-msgno
                 WITH lo_exception->if_t100_message~t100key-attr1
                      lo_exception->if_t100_message~t100key-attr2
                      lo_exception->if_t100_message~t100key-attr3
                      lo_exception->if_t100_message~t100key-attr4
                 INTO me->mv_dummy.
              me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
              lv_error = abap_true.
          ENDTRY.

          CHECK lv_error IS INITIAL.

          REFRESH: lt_services, lt_serv_line.
          lt_services[] = me->mt_po[].

          "Create Entry sheet with only service lines assigned to
          DELETE lt_services WHERE ebeln NE <fs_po>-ebeln OR
                                   ebelp NE <fs_po>-ebelp.

          LOOP AT lt_services INTO lw_services.
            MOVE-CORRESPONDING lw_services TO lw_serv_line.
            lw_serv_line-menge_iv = lw_services-menge.
            lw_serv_line-dmbtr_iv = lw_services-dmbtr.
            APPEND lw_serv_line TO lt_serv_line.
          ENDLOOP.

          me->execute_ml81n_create( EXPORTING it_serv_line  = lt_serv_line
                                    IMPORTING ev_entrysheet = lv_entrysheet
                                              ev_acceptance = lv_acceptance ).

          IF lv_entrysheet IS NOT INITIAL.

            IF lv_acceptance IS NOT INITIAL.
              MESSAGE s313(/tcsr/msg) "Criada e aceita a Folha Registro de Serviço
                 INTO me->mv_dummy WITH lv_entrysheet.
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
            ELSE.
              MESSAGE s302(/tcsr/msg) "Criada Folha Registro de Serviço & - pendente aceitação
                 INTO me->mv_dummy WITH lv_entrysheet.
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
            ENDIF.

            LOOP AT lt_serv_line INTO lw_serv_line.

              "Update Entry Sheet created
              APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
              <fs_po>-guid_header = me->mv_guid_header.
              <fs_po>-ebeln       = lw_serv_line-ebeln.
              <fs_po>-ebelp       = lw_serv_line-ebelp.
              <fs_po>-doc_number  = lv_entrysheet.
              <fs_po>-extrow      = lw_serv_line-extrow.
              <fs_po>-vgabe       = '9'.
              <fs_po>-menge       = lw_serv_line-menge_iv.
              <fs_po>-dmbtr       = lw_serv_line-dmbtr_iv.


              "Delete entry without entry sheet
              APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
              <fs_po_del>-guid_header = me->mv_guid_header.
              <fs_po_del>-ebeln       = lw_serv_line-ebeln.
              <fs_po_del>-ebelp       = lw_serv_line-ebelp.
              <fs_po_del>-extrow      = lw_serv_line-extrow.

            ENDLOOP.

          ENDIF.

        ELSE.
          MESSAGE s316(/tcsr/msg) "Service Entry Sheet was not assigned
             INTO me->mv_dummy.
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
          lv_error = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.

    CHECK lv_error IS INITIAL.
    me->mt_po_upd[] = me->mt_po[].

    me->execute_prereq_ml81n( ).

  ENDMETHOD.


  METHOD EXECUTE_ML81N_CREATE.

"*---> 28/06/2023 - Migração S4
*    DATA:
*      lt_po_items    TYPE TABLE OF bapiekpo,
*      lt_po_services TYPE TABLE OF bapiesll,
*      lt_po_return   TYPE TABLE OF bapireturn,
    DATA:
      lt_po_items    TYPE TABLE OF BAPIMEPOITEM,
      lt_po_services TYPE TABLE OF BAPIESLLC,
      lt_po_return   TYPE TABLE OF BAPIRET2,
"*---> 28/06/2023 - Migração S4
      "BAPI
      lt_bapi_eskn   TYPE TABLE OF bapiesknc,
      lt_bapi_esll   TYPE TABLE OF bapiesllc,
      lt_bapi_eskl   TYPE TABLE OF bapiesklc,
      lt_bapi_return TYPE TABLE OF bapiret2.

"*---> 28/06/2023 - Migração S4 - LO
    DATA:
*      lw_po_header   TYPE bapiekkol,
*      lw_po_items    TYPE bapiekpo,
*      lw_po_services TYPE bapiesll,
*      lw_po_return   TYPE bapireturn,
      lw_po_header   TYPE BAPIMEPOHEADER,
      lw_po_items    TYPE BAPIMEPOITEM,
      lw_po_services TYPE BAPIESLLC,
      lw_po_return   TYPE BAPIRET2,
"*---> 28/06/2023 - Migração S4 - LO
      lw_serv_line   TYPE /tcsr/s_po_list,
      "BAPI
      lw_bapi_essr   TYPE bapiessrc,
      lw_bapi_esll   TYPE bapiesllc,
      lw_bapi_return TYPE bapiret2,
      lw_out_return  TYPE bapireturn1,
      "BalLog
      lw_log_handle  TYPE balloghndl.
    DATA:
      lv_strlen         TYPE i,
      lv_line_no        TYPE bapiesllc-line_no,
      lv_pckg_no        TYPE bapiesll-pckg_no,
      lv_subpckg_no     TYPE bapiesll-subpckg_no,
      lv_gross_val_unit TYPE bapiesll-gross_val,
      lv_ref_doc_no     TYPE bapiessrc-ref_doc_no,
      lv_acceptance     TYPE bapiessr-acceptance,
      lv_entrysheet     TYPE bapiessr-sheet_no,
      lv_packno         TYPE esll-packno.
    FIELD-SYMBOLS:
      <fs_po_list> TYPE /tcsr/s_po_list.
    DATA:
      lo_bal_log TYPE REF TO /tcsr/c_bal_log,
      lo_param   TYPE REF TO /tcsr/c_param.

*------------------------------------------------

    READ TABLE it_serv_line INTO lw_serv_line INDEX 1.

"*---> 28/06/2023 - Migração S4 - LO
*    CALL FUNCTION 'BAPI_PO_GETDETAIL'"#EC CI_USAGE_OK[2438131]
*      EXPORTING
*        purchaseorder    = lw_serv_line-ebeln
*        items            = 'X'
*        services         = 'X'
*      IMPORTING
*        po_header        = lw_po_header
*      TABLES
*        po_items         = lt_po_items
*        po_item_services = lt_po_services
*        return           = lt_po_return.

    CALL FUNCTION 'BAPI_PO_GETDETAIL1'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        purchaseorder           = lw_serv_line-ebeln
       SERVICES                 = 'X'
     IMPORTING
       POHEADER                 = lw_po_header
     TABLES
       RETURN                   = lt_po_return
       POITEM                   = lt_po_items
       POSERVICES               = lt_po_services
              .
    "*---> 28/06/2023 - Migração S4 - LO

    CREATE OBJECT lo_param.
    lv_acceptance   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                               iv_name     = 'CREATE_FRS_ACEPT' ).

*--- Entry sheet header
    READ TABLE lt_po_items INTO lw_po_items WITH KEY po_item = lw_serv_line-ebelp. "<fs_po_list>-ebelp.
    IF sy-subrc EQ 0.

"*---> 01/07/2023 - Migração S4 - LO
*      lw_bapi_essr-po_number    = lw_po_items-po_number.
*      lw_bapi_essr-po_item      = lw_po_items-po_item.

      lw_bapi_essr-po_number    = lw_po_header-po_number. "nessa Bapi o numero do documento de compra só está no header
      lw_bapi_essr-po_item      = lw_po_items-po_item.
"*---> 01/07/2023 - Migração S4 - LO
      lw_bapi_essr-acceptance   = lv_acceptance.

      lw_bapi_essr-doc_date     = lw_po_header-doc_date.
      lw_bapi_essr-post_date    = sy-datum.

      IF lw_po_items-acctasscat = 'U'.
        lw_bapi_essr-accasscat  = 'K'.
      ELSE.
        lw_bapi_essr-accasscat  = lw_po_items-acctasscat.
      ENDIF.

      lw_bapi_essr-pckg_no      = lw_po_items-pckg_no.


*---- Create Ref.Doc.No
      lv_ref_doc_no = me->mw_hd-nfse_numero.
      SHIFT lv_ref_doc_no LEFT DELETING LEADING '0'.
*      "Serie
*      CONCATENATE lv_ref_doc_no '-' me->mw_hd-nfse_serie INTO lv_ref_doc_no.   "-14.04.2020
*---- Create Ref.Doc.No
*      "Serie
*      IF me->mw_hd-nfse_serie IS NOT INITIAL.
*        CONCATENATE lv_ref_doc_no '-' me->mw_hd-nfse_serie INTO lv_ref_doc_no.
*      ELSE.
*        CONCATENATE lv_ref_doc_no '-1' INTO lv_ref_doc_no.
*      ENDIF.

      lw_bapi_essr-ref_doc_no   = lv_ref_doc_no.

*--- Services items
      READ TABLE lt_po_services INTO lw_po_services WITH KEY pckg_no = lw_po_items-pckg_no.
      IF sy-subrc EQ 0.
        CLEAR: lw_bapi_esll.
        lw_bapi_esll-pckg_no    = lw_po_services-pckg_no.
        lw_bapi_esll-line_no    = lw_po_services-line_no.
        lw_bapi_esll-outl_ind   = lw_po_services-outl_ind.
        lw_bapi_esll-subpckg_no = lw_po_services-subpckg_no.
        lw_bapi_esll-from_line  = lw_po_services-from_line.
        APPEND lw_bapi_esll TO lt_bapi_esll.

        "Set SubPackno
        lv_subpckg_no = lw_po_services-subpckg_no.

        LOOP AT lt_po_services INTO lw_po_services WHERE pckg_no = lv_subpckg_no.

          "Verify if service line was assigned to create
          READ TABLE it_serv_line INTO lw_serv_line WITH KEY extrow = lw_po_services-ext_line.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CLEAR: lw_bapi_esll.

          MOVE-CORRESPONDING lw_po_services TO lw_bapi_esll.
          lw_bapi_esll-pln_pckg   = lw_bapi_esll-pckg_no.
          lw_bapi_esll-pln_line   = lw_bapi_esll-line_no.

          lw_bapi_esll-quantity   = lw_serv_line-menge_iv.

          APPEND lw_bapi_esll TO lt_bapi_esll.

        ENDLOOP.

      ENDIF.

      REFRESH: lt_bapi_return[].

      CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
        EXPORTING
          entrysheetheader            = lw_bapi_essr
          testrun                     = ''
        IMPORTING
          entrysheet                  = lv_entrysheet
        TABLES
          entrysheetaccountassignment = lt_bapi_eskn
          entrysheetservices          = lt_bapi_esll
          entrysheetsrvaccassvalues   = lt_bapi_eskl
          return                      = lt_bapi_return.

      READ TABLE lt_bapi_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc NE 0 AND lv_entrysheet IS NOT INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        DO 5 TIMES.
          "Select Serv.EntrySheet Created
          SELECT SINGLE packno
            INTO ev_packno
            FROM essr
            WHERE lblni = lv_entrysheet.
          IF sy-subrc EQ 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.

        "Return Serv.EntrySheet Number
        ev_entrysheet = lv_entrysheet.
*        "Return Acceptance flag
        ev_acceptance = lw_bapi_essr-acceptance.

        CALL FUNCTION 'MS_RESET_STORAGE_LIMITS'.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        DELETE lt_bapi_return WHERE type NE 'E'.

        IF lt_bapi_return[] IS NOT INITIAL.

          "Create BAL LOG
          CREATE OBJECT lo_bal_log.
          CALL METHOD lo_bal_log->bal_log_create
            EXPORTING
              iv_guid_header = me->mv_guid_header
            IMPORTING
              ew_log_handle  = lw_log_handle.

          "Insert All Erro Messags
          LOOP AT lt_bapi_return INTO lw_bapi_return.
            "Set Error Messages
            MESSAGE ID lw_bapi_return-id
               TYPE lw_bapi_return-type
             NUMBER lw_bapi_return-number
               WITH lw_bapi_return-message_v1
                    lw_bapi_return-message_v2
                    lw_bapi_return-message_v3
                    lw_bapi_return-message_v4
               INTO me->mv_dummy.
            "Add BAL LOG - Msg
            CALL METHOD lo_bal_log->bal_log_msg_add
              EXPORTING
                iw_log_handle = lw_log_handle.
          ENDLOOP.
          "Save BAL LOG
          CALL METHOD lo_bal_log->bal_db_save
            EXPORTING
              iw_log_handle = lw_log_handle.

        ENDIF.
        MESSAGE s308(/tcsr/msg) "Erro ao criar Folha Registro de Serviço
           WITH ''
           INTO me->mv_dummy.

        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
        EXIT.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_ML81N_DELETE.

    TYPES: BEGIN OF ty_essr,
             lblni TYPE essr-lblni,
             kzabn TYPE essr-kzabn,
           END OF ty_essr.
    DATA:
      lt_shdb_messages TYPE TABLE OF bdcmsgcoll,
      lt_return        TYPE TABLE OF bapiret2,
      lt_doc_sheet     TYPE TABLE OF /tcsr/t_ref,
      lt_essr          TYPE TABLE OF ty_essr.
    DATA:
      lw_shdb_messages TYPE bdcmsgcoll,
      lw_return        TYPE bapiret2,
      lw_log_handle    TYPE balloghndl,
      lw_essr          TYPE ty_essr.
    DATA:
      lv_kzabn    TYPE essr-kzabn.
    DATA:
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.

    FIELD-SYMBOLS: <fs_po_upd> TYPE /tcsr/t_po.
*------------------------------------------------

    CHECK: me->mt_po[] IS NOT INITIAL.

    "Check if Serv.EntrySheet is released
    SELECT lblni
           kzabn
      INTO TABLE lt_essr
      FROM essr
      FOR ALL ENTRIES IN me->mt_po
      WHERE lblni = me->mt_po-doc_number.

    LOOP AT lt_essr INTO lw_essr.

      REFRESH lt_shdb_messages.

      "Check if Serv.EntrySheet is RELEASED
      IF  lw_essr-kzabn EQ 'X'.

        "Create BAL LOG
        CREATE OBJECT lo_bal_log.
        CALL METHOD lo_bal_log->bal_log_create
          EXPORTING
            iv_guid_header = me->mv_guid_header
          IMPORTING
            ew_log_handle  = lw_log_handle.

        "Not possible to delete already accepted entry sheets
        MESSAGE e106(se) INTO me->mv_dummy.

        "Add BAL LOG - Msg
        CALL METHOD lo_bal_log->bal_log_msg_add
          EXPORTING
            iw_log_handle = lw_log_handle.

        "Save BAL LOG
        CALL METHOD lo_bal_log->bal_db_save
          EXPORTING
            iw_log_handle = lw_log_handle.

        "Error to delete Service Entry Sheet &
        MESSAGE e306(/tcsr/msg) INTO me->mv_dummy WITH lw_essr-lblni.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
        EXIT.


*        "ML81N REVOKE ACCEPTANCE
*        lt_shdb_messages = me->call_shdb_ml81n_revoke( lw_essr-lblni ).
*
*        "Check if SHDB - error
*        DELETE lt_shdb_messages WHERE msgtyp NE 'E'.
*        IF lt_shdb_messages[] IS NOT INITIAL.
*
*          "Create BAL LOG
*          CREATE OBJECT lo_bal_log.
*          CALL METHOD lo_bal_log->bal_log_create
*            EXPORTING
*              iv_guid_header = me->mv_guid_header
*            IMPORTING
*              ew_log_handle  = lw_log_handle.
*
*          "Insert All error messages
*          LOOP AT lt_shdb_messages INTO lw_shdb_messages.
*            "Set Messages
*            MESSAGE ID lw_shdb_messages-msgid
*               TYPE lw_shdb_messages-msgtyp
*             NUMBER lw_shdb_messages-msgnr
*               WITH lw_shdb_messages-msgv1
*                    lw_shdb_messages-msgv2
*                    lw_shdb_messages-msgv3
*                    lw_shdb_messages-msgv4
*               INTO me->mv_dummy.
*            "Add BAL LOG - Msg
*            CALL METHOD lo_bal_log->bal_log_msg_add
*              EXPORTING
*                iw_log_handle = lw_log_handle.
*          ENDLOOP.
*
*          "Save BAL LOG
*          CALL METHOD lo_bal_log->bal_db_save
*            EXPORTING
*              iw_log_handle = lw_log_handle.
*
*          MESSAGE s305(/tcsr/msg) "Erro ao estornar aceitação da Folha Registro de Serviço &
*             INTO me->mv_dummy WITH lw_essr-lblni.
*          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
*          CONTINUE.
*
*        ELSE.
*
*          "Check if DesAccept flag was commited
*          DO 5 TIMES.
*            "Check if Serv.EntrySheet is released
*            SELECT SINGLE kzabn
*              INTO lv_kzabn
*              FROM essr
*             WHERE lblni = lw_essr-lblni.
*            IF lv_kzabn IS INITIAL.
*              EXIT.
*            ELSE.
*              WAIT UP TO 1 SECONDS.
*            ENDIF.
*          ENDDO.
*
*        ENDIF.

      ENDIF.

      "----------------------------------------
      " DELETE SERV.ENTRYSHEET
      "----------------------------------------
      CALL FUNCTION 'BAPI_ENTRYSHEET_DELETE'
        EXPORTING
          entrysheet = lw_essr-lblni
        TABLES
          return     = lt_return.

      DELETE lt_return WHERE type NE 'E'.

      IF lt_return[] IS NOT INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        "Create BAL LOG
        CREATE OBJECT lo_bal_log.
        CALL METHOD lo_bal_log->bal_log_create
          EXPORTING
            iv_guid_header = me->mv_guid_header
          IMPORTING
            ew_log_handle  = lw_log_handle.

        "Insert All Erro Messags
        LOOP AT lt_return INTO lw_return.
          "Set Error Messages
          MESSAGE ID lw_return-id
             TYPE lw_return-type
           NUMBER lw_return-number
             WITH lw_return-message_v1
                  lw_return-message_v2
                  lw_return-message_v3
                  lw_return-message_v4
             INTO me->mv_dummy.
          "Add BAL LOG - Msg
          CALL METHOD lo_bal_log->bal_log_msg_add
            EXPORTING
              iw_log_handle = lw_log_handle.
        ENDLOOP.

        "Save BAL LOG
        CALL METHOD lo_bal_log->bal_db_save
          EXPORTING
            iw_log_handle = lw_log_handle.

        MESSAGE e306(/tcsr/msg) "Erro ao eliminar Folha Registro de Serviço &
           INTO me->mv_dummy WITH lw_essr-lblni.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
        EXIT.

      ELSE.

        MESSAGE s307(/tcsr/msg)   "Eliminada Folha Registro de Serviço *
          INTO me->mv_dummy WITH lw_essr-lblni.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-reversedoc ).

        "Select PO/item to be deleted of Process
        SELECT *
          APPENDING TABLE me->mt_po_del
          FROM /tcsr/t_po
          WHERE guid_header = me->mv_guid_header
            AND doc_number  = lw_essr-lblni.

        LOOP AT me->mt_po_del INTO mw_po.
          "Update PO table to remove Entry Sheet and Keep the PO assigned.
          APPEND INITIAL LINE TO me->mt_po_upd ASSIGNING <fs_po_upd>.
          <fs_po_upd>-guid_header = me->mv_guid_header.
          <fs_po_upd>-ebeln       = mw_po-ebeln.
          <fs_po_upd>-ebelp       = mw_po-ebelp.
          <fs_po_upd>-extrow      = mw_po-extrow.
          <fs_po_upd>-menge       = mw_po-menge.
          <fs_po_upd>-dmbtr       = mw_po-dmbtr.
        ENDLOOP.

      ENDIF. "lt_return[] IS NOT INITIAL.

    ENDLOOP.

  ENDMETHOD.


  METHOD EXECUTE_ML81N_SAVE.

    DATA:
      lt_po_list     TYPE /tcsr/y_po_list,
      lt_esll        TYPE TABLE OF esll,
      lt_esll_pckg   TYPE TABLE OF esll,
      lt_serv_line   TYPE /tcsr/y_po_list.
    DATA:
      lw_po_list_unass TYPE /tcsr/s_po_list,
      lw_po_list_assig TYPE /tcsr/s_po_list,
      lw_po_list       TYPE /tcsr/s_po_list,
      lw_serv_line     TYPE /tcsr/s_po_list,
      lw_po            TYPE /tcsr/t_po,
      lw_esll          TYPE esll.
    DATA:
      lv_entrysheet      TYPE bapiessr-sheet_no,
      lv_acceptance      TYPE bapiessr-acceptance,
      lv_create_frs_auto TYPE c,
      lv_packno          TYPE packno,
      lv_tabix           TYPE sy-tabix.
    FIELD-SYMBOLS:
      <fs_po_list_unass> TYPE /tcsr/s_po_list,
      <fs_po_list_assig> TYPE /tcsr/s_po_list,
      <fs_po>            TYPE /tcsr/t_po,
      <fs_po_del>        TYPE /tcsr/t_po.
    DATA:
      lo_param     TYPE REF TO /tcsr/c_param,
      lo_exception TYPE REF TO /tcsr/cx_exception.

*------------------------------------------------

    CREATE OBJECT lo_param.


    "--------------------------------------------
    " CHECK: IF SES was UNASSIGNED
    "--------------------------------------------
    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass WHERE lblni IS NOT INITIAL.

*      "Check SES is assigned
      READ TABLE me->mt_po_list_assig INTO lw_po_list_assig WITH KEY lblni = lw_po_list_unass-lblni
                                                                     extrow = lw_po_list_unass-extrow.
      CHECK: sy-subrc NE 0.

      MESSAGE s301(/tcsr/msg) INTO me->mv_dummy "Desassociada Folha Registro de Serviço & Linha &
                              WITH lw_po_list_unass-lblni lw_po_list_unass-extrow.
      me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-unassigned ).

      "Append PO/ITEM UNASSIGNED
      APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
      <fs_po_del>-guid_header = me->mv_guid_header.
      <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
      <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
      <fs_po_del>-vgabe       = '9'.
      <fs_po_del>-doc_number  = lw_po_list_unass-lblni.
      <fs_po_del>-extrow      = lw_po_list_unass-extrow.

      READ TABLE me->mt_po TRANSPORTING NO FIELDS WITH KEY guid_header = me->mv_guid_header
                                                           ebeln       = lw_po_list_unass-ebeln
                                                           ebelp       = lw_po_list_unass-ebelp
                                                           vgabe       = '9'
                                                           doc_number  = lw_po_list_unass-lblni
                                                           extrow      = lw_po_list_unass-extrow.
      IF sy-subrc = 0.
        DELETE me->mt_po INDEX sy-tabix.
      ENDIF.

      "Update PO table to remove Entry Sheet
      APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
      <fs_po>-guid_header = me->mv_guid_header.
      <fs_po>-ebeln       = lw_po_list_unass-ebeln.
      <fs_po>-ebelp       = lw_po_list_unass-ebelp.
      <fs_po>-extrow      = lw_po_list_unass-extrow.
      <fs_po>-menge       = lw_po_list_unass-menge_iv.
      <fs_po>-dmbtr       = lw_po_list_unass-dmbtr_iv.

      CLEAR: me->mw_act-tolerance.

    ENDLOOP.


    "--------------------------------------------
    " CHECK: IF PO was UNASSIGNED
    "--------------------------------------------
    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass WHERE lblni IS INITIAL AND
                                                             ebeln IS NOT INITIAL.

      READ TABLE me->mt_po TRANSPORTING NO FIELDS WITH KEY guid_header = me->mv_guid_header
                                                           ebeln       = lw_po_list_unass-ebeln
                                                           ebelp       = lw_po_list_unass-ebelp
                                                           extrow      = lw_po_list_unass-extrow.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
      ENDIF.

      "Check PO is assigned
      READ TABLE me->mt_po_list_assig INTO lw_po_list_assig WITH KEY ebeln = lw_po_list_unass-ebeln
                                                                     ebelp = lw_po_list_unass-ebelp
                                                                     extrow = lw_po_list_unass-extrow.
      IF sy-subrc NE 0.

        MESSAGE s311(/tcsr/msg) INTO me->mv_dummy "Desassociado Pedido de Compra &, item & e linha de serviço &
                                WITH lw_po_list_unass-ebeln lw_po_list_unass-ebelp lw_po_list_unass-extrow.
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-unassigned ).

        "Append PO/ITEM UNASSIGNED
        APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
        <fs_po_del>-guid_header = me->mv_guid_header.
        <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
        <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
        <fs_po_del>-extrow      = lw_po_list_unass-extrow.

        DELETE me->mt_po INDEX lv_tabix.

        CLEAR: me->mw_act-tolerance.

        "Assigning Entry Sheet, so remove PO/ITEM regster
      ELSEIF lw_po_list_assig-lblni IS NOT INITIAL.
        APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
        <fs_po_del>-guid_header = me->mv_guid_header.
        <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
        <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
        <fs_po_del>-extrow      = lw_po_list_unass-extrow.

        DELETE me->mt_po INDEX lv_tabix.

      ENDIF.

    ENDLOOP.


    IF me->mt_po_list_assig[] IS INITIAL.
      MESSAGE s317(/tcsr/msg) INTO me->mv_dummy.      "No PO and/or Entry Sheet assigned
      me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-pending ).
      EXIT.
    ENDIF.

    SORT me->mt_po_list_assig BY ebeln ebelp lblni extrow.

    "--------------------------------------------
    " CHECK if PO or PO/SES - was set from Monitor
    "--------------------------------------------
    LOOP AT me->mt_po_list_assig INTO me->mw_po_list_assig.

      "Check if CREATE SES
      IF me->mw_po_list_assig-lblni IS NOT INITIAL.

        "----------------------------------------
        "   ASSIGNED SES
        "----------------------------------------
        IF me->mv_action = /tcsr/c_constants=>mc_action-save.

          READ TABLE me->mt_po TRANSPORTING NO FIELDS WITH KEY guid_header = me->mv_guid_header
                                                               ebeln       = me->mw_po_list_assig-ebeln
                                                               ebelp       = me->mw_po_list_assig-ebelp
                                                               vgabe       = '9'
                                                               doc_number  = me->mw_po_list_assig-lblni
                                                               extrow      = me->mw_po_list_assig-extrow.
          IF sy-subrc NE 0.
            APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
            <fs_po>-guid_header = me->mv_guid_header.
            <fs_po>-ebeln       = me->mw_po_list_assig-ebeln.
            <fs_po>-ebelp       = me->mw_po_list_assig-ebelp.
            <fs_po>-vgabe       = '9'.
            <fs_po>-doc_number  = me->mw_po_list_assig-lblni.
            <fs_po>-extrow      = me->mw_po_list_assig-extrow.
            <fs_po>-menge       = me->mw_po_list_assig-menge_iv.
            <fs_po>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.
          ENDIF.
        ENDIF.
        "----------------------------------------
        "   ASSIGNED PO
        "----------------------------------------
      ELSE.
        READ TABLE me->mt_po TRANSPORTING NO FIELDS WITH KEY guid_header = me->mv_guid_header
                                                             ebeln       = me->mw_po_list_assig-ebeln
                                                             ebelp       = me->mw_po_list_assig-ebelp
                                                             extrow      = me->mw_po_list_assig-extrow.
        IF sy-subrc NE 0.
          "Assign PO
          APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
          <fs_po>-guid_header = me->mv_guid_header.
          <fs_po>-ebeln       = me->mw_po_list_assig-ebeln.
          <fs_po>-ebelp       = me->mw_po_list_assig-ebelp.
          <fs_po>-extrow      = me->mw_po_list_assig-extrow.
          <fs_po>-menge       = me->mw_po_list_assig-menge_iv.
          <fs_po>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.
        ENDIF.
      ENDIF.

    ENDLOOP.


    lt_po_list[] = me->mt_po_list_assig[].
    SORT lt_po_list BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM lt_po_list COMPARING ebeln ebelp.

    TRY.
        "Execute prerequisites for ML81N
        me->execute_prereq_ml81n( EXPORTING iv_prereq_value = abap_true ).

      CATCH /tcsr/cx_exception INTO lo_exception.
        EXIT.
    ENDTRY.

    LOOP AT lt_po_list INTO lw_po_list.


      "----------------------------------------
      "   ASSIGN SES(ML81N)
      "----------------------------------------
      IF lw_po_list-lblni IS NOT INITIAL.

        MESSAGE s303(/tcsr/msg) "Assigned Service Entry Sheet &
           INTO me->mv_dummy WITH lw_po_list-lblni.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).



        "----------------------------------------
        "   CREATE SES(ML81N)
        "----------------------------------------
        "Or running automatically
      ELSEIF me->mv_not_create IS INITIAL.    "Create FRS only if user has accepted

        REFRESH lt_serv_line.
        lt_serv_line[] = me->mt_po_list_assig[].

        "Create Entry sheet with only service lines assigned to
        DELETE lt_serv_line WHERE ebeln NE lw_po_list-ebeln OR
                                  ebelp NE lw_po_list-ebelp.

        me->execute_ml81n_create( EXPORTING  it_serv_line  = lt_serv_line[]
                                  IMPORTING  ev_entrysheet = lv_entrysheet
                                             ev_acceptance = lv_acceptance
                                             ev_packno     = lv_packno ).

        IF lv_entrysheet IS NOT INITIAL.

          IF lv_acceptance IS NOT INITIAL.
            MESSAGE s313(/tcsr/msg) "Criada e aceita a Folha Registro de Serviço
               INTO me->mv_dummy WITH lv_entrysheet.
            me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
          ELSE.
            MESSAGE s302(/tcsr/msg) "Criada Folha Registro de Serviço & - pendente aceitação
               INTO me->mv_dummy WITH lv_entrysheet.
            me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
          ENDIF.

          SELECT *
            INTO TABLE lt_esll_pckg
            FROM esll
            WHERE packno = lv_packno.
          IF sy-subrc = 0.
            SELECT *
              INTO TABLE lt_esll
              FROM esll
              FOR ALL ENTRIES IN lt_esll_pckg
              WHERE packno = lt_esll_pckg-sub_packno.

            "Remove lines without SES
            LOOP AT me->mt_po ASSIGNING <fs_po> WHERE ebeln = lw_po_list-ebeln AND
                                                      ebelp = lw_po_list-ebelp.
              lv_tabix = sy-tabix.
              "Delete entry without entry sheet
              APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
              <fs_po_del>-guid_header = me->mv_guid_header.
              <fs_po_del>-ebeln       = <fs_po>-ebeln.
              <fs_po_del>-ebelp       = <fs_po>-ebelp.
              <fs_po_del>-extrow      = <fs_po>-extrow.

              DELETE me->mt_po INDEX lv_tabix.
            ENDLOOP.

            LOOP AT lt_esll INTO lw_esll.
              APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
              <fs_po>-guid_header = me->mv_guid_header.
              <fs_po>-ebeln       = lw_po_list-ebeln.
              <fs_po>-ebelp       = lw_po_list-ebelp.
              <fs_po>-vgabe       = '9'.
              <fs_po>-doc_number  = lv_entrysheet.
              <fs_po>-extrow      = lw_esll-extrow.
            ENDLOOP.
          ENDIF.


          "Execute prerequisites for ML81N
          me->execute_prereq_ml81n(
                           EXPORTING
                             iv_no_success_msg = abap_true ).

        ENDIF.
      ELSE.
        MESSAGE s318(/tcsr/msg) "No Entry Sheet assigned/created
           INTO me->mv_dummy.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-pending ).

      ENDIF.

    ENDLOOP.

    me->mt_po_upd[] = me->mt_po[].

  ENDMETHOD.


  METHOD EXECUTE_MR8M_REVERSE.
    TYPES: BEGIN OF ty_rseg,
             belnr TYPE rseg-belnr,
             gjahr TYPE rseg-gjahr,
             buzei TYPE rseg-buzei,
             ebeln TYPE rseg-ebeln,
             ebelp TYPE rseg-ebelp,
           END OF ty_rseg.
    DATA:
      lt_rseg   TYPE TABLE OF ty_rseg,
      lt_return TYPE TABLE OF bapiret2.
    DATA:
      lw_rseg       TYPE ty_rseg,
      lw_return     TYPE bapiret2,
      lw_log_handle TYPE balloghndl.
    DATA:
      lv_invoice     TYPE bapi_incinv_fld-inv_doc_no,
      lv_fyear       TYPE bapi_incinv_fld-fisc_year,
      lv_postingdate TYPE bapi_incinv_fld-pstng_date,
      lv_invoice_rev TYPE bapi_incinv_fld-inv_doc_no,
      lv_fyear_rev   TYPE bapi_incinv_fld-fisc_year,
      lv_lines       TYPE i.
    DATA:
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.
*-------------------------------------------

    "Refresh Global datas from BAPI
    CALL METHOD me->call_refresh_bapis.

    lv_invoice  = me->mw_act-belnr.
    lv_fyear    = me->mw_act-gjahr.

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
      EXPORTING
        invoicedocnumber          = lv_invoice
        fiscalyear                = lv_fyear
        reasonreversal            = '01'
        postingdate               = lv_postingdate
      IMPORTING
        invoicedocnumber_reversal = lv_invoice_rev
        fiscalyear_reversal       = lv_fyear_rev
      TABLES
        return                    = lt_return.

    IF lv_invoice_rev IS NOT INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 5 TIMES.
        "Select Detail of Invoice document Created
        SELECT belnr
               gjahr
               buzei
               ebeln
               ebelp
          FROM rseg
          INTO TABLE lt_rseg
         WHERE belnr = lv_invoice_rev
           AND gjahr = lv_fyear_rev.
        IF sy-subrc EQ 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

      CLEAR: me->mw_act-belnr,
             me->mw_act-gjahr.

      MESSAGE s276(m8) INTO me->mv_dummy    "Documento estornado sob nº &
                       WITH lv_invoice_rev.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-reversed ).
      EXIT.

    ELSE.

      "Create BAL LOG
      CREATE OBJECT lo_bal_log.
      CALL METHOD lo_bal_log->bal_log_create
        EXPORTING
          iv_guid_header = me->mv_guid_header
        IMPORTING
          ew_log_handle  = lw_log_handle.

      DELETE lt_return WHERE type NE 'E'.

      READ TABLE lt_return INTO lw_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.

        LOOP AT lt_return INTO lw_return WHERE type = 'E'.
          MESSAGE ID lw_return-id
             TYPE lw_return-type
           NUMBER lw_return-number
             WITH lw_return-message_v1
                  lw_return-message_v2
                  lw_return-message_v3
                  lw_return-message_v4 INTO me->mv_dummy.
          "Add BAL LOG - Msg
          CALL METHOD lo_bal_log->bal_log_msg_add
            EXPORTING
              iw_log_handle = lw_log_handle.
        ENDLOOP.

      ELSE.

        MESSAGE e279(m8) INTO me->mv_dummy.   "Ocorreu um erro ao estornar
        "Add BAL LOG - Msg
        CALL METHOD lo_bal_log->bal_log_msg_add
          EXPORTING
            iw_log_handle = lw_log_handle.

      ENDIF.

      "Save BAL LOG
      CALL METHOD lo_bal_log->bal_db_save
        EXPORTING
          iw_log_handle = lw_log_handle.

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_PORTAL_CONTINUE.

    DATA:
      lw_ptfile  TYPE /tcsr/t_ptfile.
*-----------------------------------------------------------------------

    "Check Table - Assigned Document from Portal Vendor
    SELECT SINGLE *
      INTO lw_ptfile
      FROM /tcsr/t_ptfile
      WHERE guid_header = me->mv_guid_header.
    IF sy-subrc EQ 0.
      "NFS-e received from portal and waiting for convertion
      MESSAGE s601(/tcsr/msg) INTO me->mv_dummy.
      me->set_stepstatus_sta_table( EXPORTING iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_PREREQ.

    DATA:
      lt_prereq     TYPE TABLE OF /tcsr/t_prereq,
      lt_return_all TYPE bapiret2_tab.
    DATA:
      lw_prereq     TYPE /tcsr/t_prereq,
      lw_log_handle TYPE balloghndl.
    DATA:
      lv_method     TYPE string.
    FIELD-SYMBOLS:
      <fs_return>   TYPE bapiret2.
    DATA:
      lo_param   TYPE REF TO /tcsr/c_param,
      lo_prereq  TYPE REF TO /tcsr/c_prereq,
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.
*------------------------------------------------

    "--------------------------------------------
    " UPDATE TOLERANCE Parameter
    "--------------------------------------------
    CREATE OBJECT: lo_param.
    "Parameter Tolerance Value ( NFS-e x SAP )
    me->mw_act-tolerance   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                      iv_name     = 'TOLERANCE' ).
    "Parameter Tolerance Tax
    me->mw_act-tolerance_tax   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                          iv_name     = 'TOLERANCE_TAX' ).

    CREATE OBJECT lo_prereq.
    lo_prereq->load_new_guid( EXPORTING iw_act = me->mw_act
                                        iw_hd  = me->mw_hd
                                        it_po  = me->mt_po ).

    "--------------------------------------------
    " Select Prerequisites Configuration
    "--------------------------------------------
    SELECT *
      INTO TABLE lt_prereq
      FROM /tcsr/t_prereq
      WHERE proctyp = me->mw_act-proctyp
        AND step    = me->mw_sta-step
        AND active  = 'X'
      ORDER BY PRIMARY KEY.
    IF sy-subrc NE 0.
      "Set Status "WARING"
      MESSAGE e221(/tcsr/msg) INTO me->mv_dummy.  "Nenhuma configuração de Pré-requisitos cadastrada
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
      EXIT.
    ENDIF.

    SORT lt_prereq BY proctyp step seqno.

    "--------------------------------------------
    " CHECK ALL Prerequisite Configurated
    "--------------------------------------------
    LOOP AT lt_prereq INTO lw_prereq.

      IF lw_prereq-prereqimpl IS INITIAL.
        APPEND INITIAL LINE TO lt_return_all ASSIGNING <fs_return>.
        <fs_return>-type        = 'E'.
        <fs_return>-id          = /tcsr/c_constants=>c_msg_class.
        <fs_return>-number      = '222'. "Método de Implementação do Pré-requisito não foi configurado
        <fs_return>-message_v1  = ''.
        <fs_return>-message_v2  = ''.
        <fs_return>-message_v3  = ''.
        <fs_return>-message_v4  = ''.
        CONTINUE.
      ENDIF.

      "Set Configurated Method name
      lv_method = lw_prereq-prereqimpl.

*------------------------------------------------
*---  CALL METHOD - PREREQ
*------------------------------------------------
      TRY.

          CALL METHOD lo_prereq->(lv_method).

          "--------------------------------------
          " GET: Return Error
          "--------------------------------------
          IF lo_prereq->mt_return_error[] IS NOT INITIAL.
            APPEND LINES OF lo_prereq->mt_return_error[] TO lt_return_all[].
            REFRESH: lo_prereq->mt_return_error[].
          ENDIF.

        CATCH cx_sy_dyn_call_error.
          APPEND INITIAL LINE TO lt_return_all ASSIGNING <fs_return>.
          <fs_return>-type        = 'E'.
          <fs_return>-id          = /tcsr/c_constants=>c_msg_class.
          <fs_return>-number      = '223'.
          <fs_return>-message_v1  = lw_prereq-prereqimpl.
          <fs_return>-message_v2  = ''.
          <fs_return>-message_v3  = ''.
          <fs_return>-message_v4  = ''.
      ENDTRY.

      lo_prereq->get_guid_data( IMPORTING ew_act = me->mw_act ).

    ENDLOOP.

    "--------------------------------------------
    " CHECK: IF PreReq were executed sucessfully
    "--------------------------------------------
    IF  lt_return_all[] IS INITIAL.

      CASE me->mw_sta-step.
        WHEN 'PREREQ'.
          "Set Status "SUCCESS"
          MESSAGE s224(/tcsr/msg) INTO me->mv_dummy.      "Pré-requisitos validados com sucesso
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
        WHEN 'POASSIGN'.
          "Set Status "SUCCESS"
          MESSAGE s342(/tcsr/msg) INTO me->mv_dummy.      "Purchase Order successfully validated
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
        WHEN 'VALIDATE'.
          "Set Status "SUCCESS"
          MESSAGE e391(/tcsr/msg) INTO me->mv_dummy.      "Processo validado com sucesso
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
        WHEN 'MIRO'.
          "Set Status "SUCCESS"
          MESSAGE e391(/tcsr/msg) INTO me->mv_dummy.      "Processo validado com sucesso
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
      ENDCASE.

    ELSE.
      rv_subrc = 4.

      CREATE OBJECT lo_bal_log.

      CALL METHOD lo_bal_log->bal_log_create
        EXPORTING
          iv_guid_header = me->mw_hd-guid_header
        IMPORTING
          ew_log_handle  = lw_log_handle.

      "--------------------------------------------
      " SAVE: Return Error Messages
      "--------------------------------------------
      LOOP AT lt_return_all ASSIGNING <fs_return>.
        MESSAGE ID <fs_return>-id
           TYPE <fs_return>-type
         NUMBER <fs_return>-number
           WITH <fs_return>-message_v1
                <fs_return>-message_v2
                <fs_return>-message_v3
                <fs_return>-message_v4
           INTO me->mv_dummy.
        CALL METHOD lo_bal_log->bal_log_msg_add
          EXPORTING
            iw_log_handle = lw_log_handle.

        CASE <fs_return>-type.
          WHEN 'E'.
            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
          WHEN 'W'.
            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-warning ).
        ENDCASE.

      ENDLOOP.

      CALL METHOD lo_bal_log->bal_db_save
        EXPORTING
          iw_log_handle = lw_log_handle.

      MESSAGE e262(/tcsr/msg) "Erro ao validar pré-requisitos (ver log detalhado)
         INTO me->mv_dummy.
      me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_PREREQ_MIGO.

    DATA:
      lt_prereq TYPE TABLE OF /tcsr/t_prereq.
    DATA:
      lw_prereq TYPE /tcsr/t_prereq.
    DATA:
      lv_method       TYPE string,
      lv_prereq_error TYPE char1. "abap_bool.
    DATA:
      lo_param     TYPE REF TO /tcsr/c_param,
      lo_prereq    TYPE REF TO /tcsr/c_prereq,
      lo_exception TYPE REF TO /tcsr/cx_exception.
*-----------------------------------------------------------------------

    "--------------------------------------------
    " UPDATE TOLERANCE Parameter
    "--------------------------------------------
    CREATE OBJECT: lo_param.
    "Parameter Tolerance Value ( NFS-e x SAP )
    me->mw_act-tolerance   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                      iv_name     = 'TOLERANCE' ).
    "Parameter Tolerance Tax
    me->mw_act-tolerance_tax   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                          iv_name     = 'TOLERANCE_TAX' ).

    "--------------------------------------------
    " CHECK ALL Prerequisite Configurated
    "--------------------------------------------
    CREATE OBJECT lo_prereq.
    lo_prereq->load_new_guid( EXPORTING iw_act = me->mw_act
                                        iw_hd  = me->mw_hd
                                        it_po  = me->mt_po
                                        it_po_list_assig = me->mt_po_list_assig ).

    IF iv_prereq_value IS INITIAL.

      "--------------------------------------------
      " Select Prerequisites Configuration
      "--------------------------------------------
      SELECT *
        INTO TABLE lt_prereq
        FROM /tcsr/t_prereq
        WHERE proctyp = me->mw_act-proctyp
          AND step    = me->mw_sta-step
          AND active  = 'X'
        ORDER BY PRIMARY KEY.
      IF sy-subrc NE 0.
        "Set Status "WARING"
        MESSAGE w315(/tcsr/msg) INTO me->mv_dummy. "Nenhum Pré-requisito cadastrado para validação do Doc.Material
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
        EXIT.
      ENDIF.

      LOOP AT lt_prereq INTO lw_prereq.

        IF lw_prereq-prereqimpl IS INITIAL.
          MESSAGE e222(/tcsc/msg) INTO me->mv_dummy         "Método de Implementação do Pré-requisito não foi configurado
                                  WITH lw_prereq-step.
          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
          EXIT.
        ENDIF.

        "Set Configurated Method name
        lv_method = lw_prereq-prereqimpl.

*------------------------------------------------
*-----  CALL METHOD - PREREQ
*------------------------------------------------
        TRY.

            CALL METHOD lo_prereq->(lv_method).

          CATCH /tcsr/cx_exception INTO lo_exception.
            lv_prereq_error = 'X'.
            "Returned Error
            MESSAGE ID lo_exception->if_t100_message~t100key-msgid
               TYPE 'E'
             NUMBER lo_exception->if_t100_message~t100key-msgno
               WITH lo_exception->if_t100_message~t100key-attr1
                    lo_exception->if_t100_message~t100key-attr2
                    lo_exception->if_t100_message~t100key-attr3
                    lo_exception->if_t100_message~t100key-attr4
               INTO me->mv_dummy.
            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
            EXIT.

          CATCH cx_sy_dyn_call_error.
            lv_prereq_error = 'X'.
            MESSAGE e223(/tcsc/msg) INTO me->mv_dummy         "Erro ao executar Pré-requisito &
                                    WITH lw_prereq-prereqimpl.
            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
            EXIT.
        ENDTRY.

      ENDLOOP.

      "--------------------------------------------
      " CHECK: IF PreReq were executed sucessfully
      "--------------------------------------------
      IF lv_prereq_error IS INITIAL AND iv_no_success_msg IS INITIAL.
        "Set Status "SUCCESS"
        MESSAGE s260(/tcsr/msg) INTO me->mv_dummy.      "Documento de Material validado com sucesso
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
      ENDIF.


    ELSE.

*------------------------------------------------
*-----  CALL METHOD - PREREQ VALUE CHECK
*------------------------------------------------
      TRY.

          CALL METHOD lo_prereq->check_values( ).

        CATCH /tcsr/cx_exception INTO lo_exception.
          mo_util_xml->set_msgkey( iv_msgno    = lo_exception->if_t100_message~t100key-msgno
                                   in_attr1    = lo_exception->if_t100_message~t100key-attr1
                                   in_attr2    = lo_exception->if_t100_message~t100key-attr2
                                   in_attr3    = lo_exception->if_t100_message~t100key-attr3
                                   in_attr4    = lo_exception->if_t100_message~t100key-attr4 ).
          RAISE EXCEPTION TYPE /tcsr/cx_exception
            EXPORTING
              textid = mo_util_xml->mw_msgkey.

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_PREREQ_ML81N.

    DATA:
      lt_prereq TYPE TABLE OF /tcsr/t_prereq.
    DATA:
      lw_prereq TYPE /tcsr/t_prereq.
    DATA:
      lv_method       TYPE string,
      lv_prereq_error TYPE char1.
    DATA:
      lo_param     TYPE REF TO /tcsr/c_param,
      lo_prereq    TYPE REF TO /tcsr/c_prereq,
      lo_exception TYPE REF TO /tcsr/cx_exception.
*-----------------------------------------------------------------------

    "--------------------------------------------
    " UPDATE TOLERANCE Parameter
    "--------------------------------------------
    CREATE OBJECT: lo_param.
    "Parameter Tolerance Value ( NFS-e x SAP )
    me->mw_act-tolerance       = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                          iv_name     = 'TOLERANCE' ).
    "Parameter Tolerance Tax
    me->mw_act-tolerance_tax   = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                          iv_name     = 'TOLERANCE_TAX' ).

    "--------------------------------------------
    " CHECK ALL Prerequisite Configurated
    "--------------------------------------------
    CREATE OBJECT lo_prereq.
    lo_prereq->load_new_guid( EXPORTING iw_act = me->mw_act
                                        iw_hd  = me->mw_hd
                                        it_po  = me->mt_po
                                        it_po_list_assig = me->mt_po_list_assig ).

    IF iv_prereq_value IS INITIAL.
      "--------------------------------------------
      " Select Prerequisites Configuration
      "--------------------------------------------
      SELECT *
        INTO TABLE lt_prereq
        FROM /tcsr/t_prereq
        WHERE proctyp = me->mw_act-proctyp
          AND step    = me->mw_sta-step
          AND active  = 'X'
        ORDER BY PRIMARY KEY.
      IF sy-subrc NE 0.
        "Set Status "WARING"
        MESSAGE e310(/tcsr/msg) INTO me->mv_dummy. "Nenhum Pré-requisito cadastrado para validação da Folha Reg. de Serviço &
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
        EXIT.
      ENDIF.

      LOOP AT lt_prereq INTO lw_prereq.

        IF lw_prereq-prereqimpl IS INITIAL.
          MESSAGE e222(/tcsr/msg) INTO me->mv_dummy         "Método de Implementação do Pré-requisito não foi configurado
                                  WITH lw_prereq-step.
          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
          EXIT.
        ENDIF.

        "Set Configurated Method name
        lv_method = lw_prereq-prereqimpl.

*------------------------------------------------
*-----  CALL METHOD - PREREQ
*------------------------------------------------
        TRY.

            CALL METHOD lo_prereq->(lv_method).

          CATCH /tcsr/cx_exception INTO lo_exception.
            lv_prereq_error = 'X'.
            "Returned Error
            MESSAGE ID lo_exception->if_t100_message~t100key-msgid
               TYPE 'E'
             NUMBER lo_exception->if_t100_message~t100key-msgno
               WITH lo_exception->if_t100_message~t100key-attr1
                    lo_exception->if_t100_message~t100key-attr2
                    lo_exception->if_t100_message~t100key-attr3
                    lo_exception->if_t100_message~t100key-attr4
               INTO me->mv_dummy.
            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
            EXIT.

          CATCH cx_sy_dyn_call_error.
            lv_prereq_error = 'X'.
            MESSAGE e223(/tcsr/msg) INTO me->mv_dummy         "Erro ao executar Pré-requisito &
                                    WITH lw_prereq-prereqimpl.
            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
            EXIT.
        ENDTRY.

      ENDLOOP.

      "--------------------------------------------
      " CHECK: IF PreReq were executed sucessfully
      "--------------------------------------------
      IF lv_prereq_error IS INITIAL AND iv_no_success_msg IS INITIAL.
        "Set Status "SUCCESS"
        MESSAGE e246(/tcsr/msg) INTO me->mv_dummy.    "Folha(s) de Serviço validada(s) com sucesso
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).
      ENDIF.


    ELSE.
*------------------------------------------------
*-----  CALL METHOD - PREREQ VALUE CHECK
*------------------------------------------------
      TRY.

          CALL METHOD lo_prereq->check_values( ).

        CATCH /tcsr/cx_exception INTO lo_exception.
          ev_prereq_error = 'X'.
          "Returned Error
          MESSAGE ID lo_exception->if_t100_message~t100key-msgid
             TYPE 'E'
           NUMBER lo_exception->if_t100_message~t100key-msgno
             WITH lo_exception->if_t100_message~t100key-attr1
                  lo_exception->if_t100_message~t100key-attr2
                  lo_exception->if_t100_message~t100key-attr3
                  lo_exception->if_t100_message~t100key-attr4
             INTO me->mv_dummy.
          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
          EXIT.

        CATCH cx_sy_dyn_call_error.
          ev_prereq_error = 'X'.
          MESSAGE e223(/tcsr/msg) INTO me->mv_dummy         "Erro ao executar Pré-requisito &
                                  WITH lw_prereq-prereqimpl.
          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
          EXIT.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_PROCDET.

    TYPES:
      BEGIN OF ty_essr,
        lblni  TYPE essr-lblni,
        ebeln  TYPE essr-ebeln,
        ebelp  TYPE essr-ebelp,
        packno TYPE essr-packno,
        aedat  TYPE essr-aedat,
      END OF ty_essr.
    TYPES:
      BEGIN OF ty_eslh,
        packno TYPE eslh-packno,
        waers  TYPE eslh-waers,
      END OF ty_eslh.

    TYPES:
      BEGIN OF ty_esll_pckg,
        packno     TYPE esll-packno,
        introw     TYPE esll-introw,
        sub_packno TYPE esll-sub_packno,
      END OF ty_esll_pckg.

    TYPES:
      BEGIN OF ty_esll,
        packno     TYPE esll-packno,
        introw     TYPE esll-introw,
        extrow     TYPE esll-extrow,
        srvpos     TYPE esll-srvpos,
        ktext1     TYPE esll-ktext1,
        sub_packno TYPE esll-sub_packno,
        menge      TYPE esll-menge,
        meins      TYPE esll-meins,
        netwr      TYPE esll-netwr,
        vrtkz      TYPE esll-vrtkz,
        act_menge  TYPE esll-act_menge,
        tbtwr      TYPE esll-tbtwr,
      END OF ty_esll.

    TYPES:
      BEGIN OF ty_ekpo,
        ebeln   TYPE ekpo-ebeln,
        ebelp   TYPE ekpo-ebelp,
        bukrs   TYPE ekko-bukrs,
        werks   TYPE ekpo-werks,
        loekz   TYPE ekko-loekz,
        bsart   TYPE ekko-bsart,
        aedat   TYPE ekko-aedat,
        ernam   TYPE ekko-ernam,
        matnr   TYPE ekpo-matnr,
        txz01   TYPE ekpo-txz01,
        menge   TYPE ekpo-menge,
        netwr   TYPE ekpo-netwr,
        j_1bnbm TYPE ekpo-j_1bnbm,
        ekorg   TYPE ekko-ekorg,
        packno  TYPE ekpo-packno,
      END OF ty_ekpo.
    TYPES:
      BEGIN OF ty_t001w,
        werks      TYPE t001w-werks,
        j_1bbranch TYPE t001w-j_1bbranch,
      END OF ty_t001w.
    TYPES:
      BEGIN OF ty_po,
        guid_header TYPE /tcsr/t_po-guid_header,
        doc_number  TYPE /tcsr/t_po-doc_number,
      END OF ty_po.
    TYPES:
      BEGIN OF ty_mseg,
        mblnr TYPE mseg-mblnr,
        mjahr TYPE mseg-mjahr,
        zeile TYPE mseg-zeile,
        matnr TYPE mseg-matnr,
        lifnr TYPE mseg-lifnr,
        menge TYPE mseg-menge,
        meins TYPE mseg-meins,
        waers TYPE mseg-waers,
        dmbtr TYPE mseg-dmbtr,
        ebeln TYPE mseg-ebeln,
        ebelp TYPE mseg-ebelp,
        sjahr TYPE mseg-sjahr,
        smbln TYPE mseg-smbln,
        smblp TYPE mseg-smblp,
      END OF ty_mseg.

    DATA:
      lt_vendor     TYPE TABLE OF /tcsr/t_vendor,
      lt_doc_po     TYPE TABLE OF /tcsr/t_proc_doc,
      lt_doc_po_aux TYPE TABLE OF /tcsr/t_proc_doc,
      lt_doc_sheet  TYPE TABLE OF /tcsr/t_proc_doc,
      lt_doc_gr     TYPE TABLE OF /tcsr/t_proc_doc,
      lt_essr       TYPE TABLE OF ty_essr,
      lt_eslh       TYPE TABLE OF ty_eslh,
      lt_esll_pckg  TYPE TABLE OF ty_esll_pckg,
      lt_esll       TYPE TABLE OF ty_esll,
      lt_ekpo       TYPE TABLE OF ty_ekpo,
      lt_ekpo_fae   TYPE TABLE OF ty_ekpo,
      lt_t001w      TYPE TABLE OF ty_t001w,
      lt_po         TYPE TABLE OF ty_po,
      lt_mseg       TYPE TABLE OF ty_mseg,
      lt_proc_po    TYPE TABLE OF ty_ekpo,
      lt_return     TYPE TABLE OF /tcsr/s_return_po_check.
    DATA:
      lw_vendor     TYPE /tcsr/t_vendor,
      lw_essr       TYPE ty_essr,
      lw_eslh       TYPE ty_eslh,
      lw_esll       TYPE ty_esll,
      lw_esll_pckg  TYPE ty_esll_pckg,
      lw_po         TYPE ty_po,
      lw_t001w      TYPE ty_t001w,
      lw_doc_po     TYPE /tcsr/t_proc_doc,
      lw_doc_sheet  TYPE /tcsr/t_proc_doc,
      lw_doc_gr     TYPE /tcsr/t_proc_doc,
      lw_ekpo       TYPE ty_ekpo,
      lw_mseg       TYPE ty_mseg,
      lw_proc_po    TYPE ty_ekpo,
      lw_return     TYPE /tcsr/s_return_po_check,
      lw_log_handle TYPE balloghndl.
    DATA:
      lv_lifnr         TYPE lfa1-lifnr,
      lv_branch        TYPE t001w-j_1bbranch,
      lv_param_proctyp TYPE /tcsr/t_proctyp-proctyp,
      lv_proctyp_old   TYPE /tcsr/t_proctyp-proctyp,
      lv_procdescr     TYPE /tcsr/t_proctypt-procdescr,
      lv_cgc_number    TYPE j_1bwfield-cgc_number,
      lv_lines         TYPE sy-tabix,
      lv_error         TYPE c.
    DATA:
      rg_lifnr TYPE RANGE OF lifnr.
    FIELD-SYMBOLS:
      <fs_range>    LIKE LINE OF rg_lifnr,
      <fs_vendor>   TYPE /tcsr/t_vendor,
      <fs_proc_doc> TYPE /tcsr/t_proc_doc,
      <fs_po>       TYPE /tcsr/t_po.
    DATA:
      lo_param   TYPE REF TO /tcsr/c_param,
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.
*------------------------------------------------

    IF mw_xml_rpsx-inf_rps-status = '2'.
      lv_param_proctyp = 'PROCCANC'.
    ELSE.

      CREATE OBJECT: lo_param.

      "Create BAL LOG
      CREATE OBJECT lo_bal_log.
      CALL METHOD lo_bal_log->bal_log_create
        EXPORTING
          iv_guid_header = me->mw_hd-guid_header
        IMPORTING
          ew_log_handle  = lw_log_handle.


      lv_lifnr = me->mw_act-lifnr.

      "Select configuration by Vendor
      SELECT *
        INTO TABLE lt_vendor
        FROM /tcsr/t_vendor
        WHERE active = 'X'.
      IF sy-subrc EQ 0.

        LOOP AT lt_vendor ASSIGNING <fs_vendor>.
          "Create LIFNR range
          APPEND INITIAL LINE TO rg_lifnr ASSIGNING <fs_range>.

          IF <fs_vendor>-lifnr_h IS INITIAL.
            <fs_range>-sign   = 'I'.
            <fs_range>-option = 'EQ'.
            <fs_range>-low    = <fs_vendor>-lifnr_l.
          ELSE.
            <fs_range>-sign   = 'I'.
            <fs_range>-option = 'BT'.
            <fs_range>-low    = <fs_vendor>-lifnr_l.
            <fs_range>-high   = <fs_vendor>-lifnr_h.
          ENDIF.

        ENDLOOP.

      ENDIF.

      "Check if Vendor was Set
      IF lv_lifnr NOT IN rg_lifnr[].
        MESSAGE e272(/tcsr/msg)     "Fornecedor & não está configurado para recebimento de NFS-e
          INTO me->mv_dummy WITH lv_lifnr.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
        EXIT.
      ENDIF.

      "Check if NFS-e: NORMAL or CANCELED
      IF me->mw_act-cancel IS INITIAL.

        IF me->mv_proctyp_manual IS NOT INITIAL.
          lv_param_proctyp = me->mv_proctyp_manual.
        ENDIF.
      ELSE.
        "Get CANCEL Process from parameter
        lv_param_proctyp = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                    iv_name     = 'PROCTYP_CANCEL' ).
        IF lv_param_proctyp IS INITIAL.
          MESSAGE e005(/tcsr/msg) "Parâmetro & não foi cadastrado
             INTO me->mv_dummy WITH 'PROCTYP_CANCEL'.
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
          EXIT.
        ENDIF.
      ENDIF.

      IF lv_param_proctyp IS INITIAL.

        IF me->mt_proc_doc[] IS NOT INITIAL.

          "Get documents informed by user
          lt_doc_sheet[] = me->mt_proc_doc[].
          DELETE lt_doc_sheet WHERE vgabe NE '9'.

          lt_doc_gr[] = me->mt_proc_doc[].
          DELETE lt_doc_gr WHERE vgabe NE '1'.

          lt_doc_po[] = me->mt_proc_doc[].
          DELETE lt_doc_po WHERE vgabe NE space.
          SORT lt_doc_po BY ebeln ebelp.

        ELSE.

          "Get documents informed by vendor
          LOOP AT me->mt_ref INTO me->mw_ref.
            CHECK me->mw_ref-doc_number IS NOT INITIAL.
            IF me->mw_ref-doc_type = 'S'.
              lw_doc_sheet-guid_header = me->mv_guid_header.
              lw_doc_sheet-vgabe       = '9'.
              lw_doc_sheet-doc_number  = me->mw_ref-doc_number.
              COLLECT lw_doc_sheet INTO lt_doc_sheet.
            ELSEIF me->mw_ref-doc_type = 'G'.
              lw_doc_gr-guid_header = me->mv_guid_header.
              lw_doc_gr-vgabe       = '1'.
              lw_doc_gr-doc_number  = me->mw_ref-doc_number.
              lw_doc_gr-gjahr       = me->mw_ref-gjahr.
              COLLECT lw_doc_gr INTO lt_doc_gr.
            ELSEIF me->mw_ref-doc_type = 'P'.
              lw_doc_po-guid_header = me->mv_guid_header.
              lw_doc_po-ebeln       = me->mw_ref-doc_number.
              COLLECT lw_doc_po INTO lt_doc_po.
            ENDIF.
          ENDLOOP.

        ENDIF.

        IF lt_doc_sheet[] IS NOT INITIAL.

          "Check if SES is already set in another Process
          SELECT p~guid_header
                 p~doc_number
            INTO TABLE lt_po
            FROM /tcsr/t_po AS p
           INNER JOIN /tcsr/t_act AS a
              ON p~guid_header = a~guid_header
            FOR ALL ENTRIES IN lt_doc_sheet
            WHERE p~doc_number      EQ lt_doc_sheet-doc_number
              AND a~last_stepstatus NE '102'. "Inutilizado
          IF sy-subrc NE 0.

            "Select SES from SAP
            SELECT lblni ebeln ebelp packno aedat
              INTO TABLE lt_essr
              FROM essr
              FOR ALL ENTRIES IN lt_doc_sheet
              WHERE lblni = lt_doc_sheet-doc_number.
            IF sy-subrc = 0.

              "Select header of Service
              SELECT packno
                     waers
                INTO TABLE lt_eslh
                FROM eslh
                FOR ALL ENTRIES IN lt_essr
                WHERE packno = lt_essr-packno
                ORDER BY PRIMARY KEY.

              "Select Sevices Header
              SELECT packno
                     introw
                     sub_packno
                INTO TABLE lt_esll_pckg
                FROM esll
                FOR ALL ENTRIES IN lt_essr
                WHERE packno = lt_essr-packno
                ORDER BY PRIMARY KEY.
              IF sy-subrc EQ 0.

                "Select Services Item
                SELECT packno
                       introw
                       extrow
                       srvpos
                       ktext1
                       sub_packno
                       menge
                       meins
                       netwr
                       vrtkz
                       act_menge
                       tbtwr
                  INTO TABLE lt_esll
                  FROM esll
                  FOR ALL ENTRIES IN lt_esll_pckg
                  WHERE packno = lt_esll_pckg-sub_packno
                  ORDER BY PRIMARY KEY.

              ENDIF.

              SELECT ekpo~ebeln
                     ekpo~ebelp
                     ekko~bukrs
                     ekpo~werks
                     ekko~loekz
                     ekko~bsart
                     ekko~aedat
                     ekko~ernam
                     ekpo~matnr
                     ekpo~txz01
                     ekpo~menge
                     ekpo~netwr
                     ekpo~j_1bnbm
                     ekko~ekorg
                     ekpo~packno
                INTO TABLE lt_ekpo
                FROM ekpo
               INNER JOIN ekko
                  ON ekko~ebeln EQ ekpo~ebeln
               FOR ALL ENTRIES IN lt_essr
               WHERE ekpo~ebeln = lt_essr-ebeln
                 AND ekpo~ebelp = lt_essr-ebelp.
              IF sy-subrc = 0.

                lt_ekpo_fae[] = lt_ekpo[].
                SORT lt_ekpo_fae BY werks.
                DELETE ADJACENT DUPLICATES FROM lt_ekpo_fae COMPARING werks.

                "CHECK: CNPJ Company/Branch x CNPJ NFS-e Tomador
                SELECT werks
                       j_1bbranch
                  INTO TABLE lt_t001w
                  FROM t001w
                  FOR ALL ENTRIES IN lt_ekpo_fae
                 WHERE werks EQ lt_ekpo_fae-werks.
              ENDIF.

            ENDIF.

          ENDIF.

          SORT lt_essr BY lblni.
          SORT lt_t001w BY werks.
          SORT lt_ekpo BY ebeln.

          me->mw_act-ref_doc_error = abap_false.
          me->mw_act-ref_doc_no = abap_false.
          LOOP AT lt_doc_sheet INTO lw_doc_sheet.

            READ TABLE lt_po INTO lw_po WITH KEY guid_header = me->mw_hd-guid_header.
            IF sy-subrc = 0.
              MESSAGE s281(/tcsr/msg) INTO me->mv_dummy       "Folha Registro de Serviço & já está sendo utilizado pelo GUID &
                                      WITH lw_po-doc_number lw_po-guid_header.
              CALL METHOD lo_bal_log->bal_log_msg_add
                EXPORTING
                  iw_log_handle = lw_log_handle.
              CALL METHOD lo_bal_log->bal_db_save
                EXPORTING
                  iw_log_handle = lw_log_handle.
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
              me->mw_act-ref_doc_error = abap_true.
              CONTINUE.
            ENDIF.

            READ TABLE lt_essr INTO lw_essr WITH KEY lblni = lw_doc_sheet-doc_number
                                                            BINARY SEARCH.
            IF sy-subrc NE 0.
              MESSAGE s285(/tcsr/msg) INTO me->mv_dummy         "Folha de Serviço & não existe no SAP
                                      WITH lw_doc_sheet-doc_number.
              CALL METHOD lo_bal_log->bal_log_msg_add
                EXPORTING
                  iw_log_handle = lw_log_handle.
              CALL METHOD lo_bal_log->bal_db_save
                EXPORTING
                  iw_log_handle = lw_log_handle.
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
              me->mw_act-ref_doc_error = abap_true.
              CONTINUE.
            ENDIF.

            READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_essr-ebeln
                                                     ebelp = lw_essr-ebelp.
            IF sy-subrc = 0.

              IF lw_ekpo-bukrs NE me->mw_act-bukrs.
                MESSAGE s289(/tcsr/msg) INTO me->mv_dummy     "Empresa & da PO & diferente da empresa do CNPJ & informado no XML
                                        WITH lw_ekpo-bukrs lw_doc_po-doc_number me->mw_hd-t_cnpj.
                CALL METHOD lo_bal_log->bal_log_msg_add
                  EXPORTING
                    iw_log_handle = lw_log_handle.
                CALL METHOD lo_bal_log->bal_db_save
                  EXPORTING
                    iw_log_handle = lw_log_handle.
                me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
                me->mw_act-ref_doc_error = abap_true.
                CONTINUE.
              ENDIF.

              READ TABLE me->mt_proc_doc ASSIGNING <fs_proc_doc> WITH KEY ebeln = lw_ekpo-ebeln
                                                                          ebelp = lw_ekpo-ebelp.
              IF sy-subrc NE 0.

                "Read Header Service
                READ TABLE lt_eslh INTO lw_eslh WITH KEY packno = lw_essr-packno
                                                              BINARY SEARCH.
                CHECK: sy-subrc EQ 0.

                "Read Header Packno
                READ TABLE lt_esll_pckg INTO lw_esll_pckg
                                        WITH KEY packno = lw_essr-packno
                                                 BINARY SEARCH.
                CHECK: sy-subrc EQ 0.

                "Read Services Item
                READ TABLE lt_esll TRANSPORTING NO FIELDS
                                   WITH KEY packno = lw_esll_pckg-sub_packno
                                            BINARY SEARCH.
                IF sy-subrc EQ 0.

                  LOOP AT lt_esll INTO lw_esll FROM sy-tabix.
                    IF lw_esll-packno NE lw_esll_pckg-sub_packno.
                      EXIT.
                    ENDIF.

                    "Save data at PO table
                    APPEND INITIAL LINE TO me->mt_proc_doc ASSIGNING <fs_proc_doc>.
                    <fs_proc_doc>-guid_header = me->mv_guid_header.
                    <fs_proc_doc>-ebeln       = lw_ekpo-ebeln.
                    <fs_proc_doc>-ebelp       = lw_ekpo-ebelp.
                    <fs_proc_doc>-vgabe       = '9'.
                    <fs_proc_doc>-doc_number  = lw_essr-lblni.
                    <fs_proc_doc>-extrow      = lw_esll-extrow.

                    "Save data at PO ASSIGN table
                    mw_po_list_assig-bukrs          = lw_ekpo-bukrs.
                    mw_po_list_assig-ekorg          = lw_ekpo-ekorg.
                    mw_po_list_assig-ernam          = lw_ekpo-ernam.
                    mw_po_list_assig-aedat          = lw_ekpo-aedat.
                    mw_po_list_assig-ebeln          = lw_ekpo-ebeln.
                    mw_po_list_assig-ebelp          = lw_ekpo-ebelp.
                    mw_po_list_assig-matnr          = lw_ekpo-matnr.
                    mw_po_list_assig-txz01          = lw_ekpo-txz01.
                    mw_po_list_assig-werks          = lw_ekpo-werks.
                    mw_po_list_assig-j_1bnbm        = lw_ekpo-j_1bnbm.
                    mw_po_list_assig-menge          = lw_ekpo-menge.
                    mw_po_list_assig-netwr          = lw_ekpo-netwr.
                    mw_po_list_assig-lblni          = lw_essr-lblni.
                    mw_po_list_assig-aedat_ses      = lw_essr-aedat.      "Change Date
                    mw_po_list_assig-lblni          = lw_essr-lblni.      "Serv.EntrySheet
                    mw_po_list_assig-packno         = lw_essr-packno.     "Packno
                    mw_po_list_assig-introw         = lw_esll-introw.     "Internal item
                    mw_po_list_assig-extrow         = lw_esll-extrow.     "External item
                    mw_po_list_assig-srvpos         = lw_esll-srvpos.     "Service number
                    mw_po_list_assig-ktext1_ses     = lw_esll-ktext1.     "Serv.Text
                    mw_po_list_assig-menge_ses      = lw_esll-menge.      "Qty
                    mw_po_list_assig-meins_ses      = lw_esll-meins.      "Unit
                    mw_po_list_assig-tbtwr_ses      = lw_esll-tbtwr.      "Gross Value
                    mw_po_list_assig-netwr_ses      = lw_esll-netwr.      "Net Value
                    mw_po_list_assig-waers_ses      = lw_eslh-waers.      "Currency
                    mw_po_list_assig-act_menge_ses  = lw_esll-menge.      "PO: Entered Quantity
                    mw_po_list_assig-distr_ses      = lw_esll-vrtkz.      "Distribution indicator
                    mw_po_list_assig-menge_iv       = lw_esll-menge.
                    mw_po_list_assig-dmbtr_iv = mw_po_list_assig-menge_iv * mw_po_list_assig-tbtwr_ses.

                    <fs_proc_doc>-menge       = me->mw_po_list_assig-menge_iv.
                    <fs_proc_doc>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.

                    APPEND mw_po_list_assig TO mt_po_list_assig.
                    CLEAR mw_po_list_assig.
                  ENDLOOP.
                ENDIF.
              ENDIF.

              COLLECT lw_ekpo INTO lt_proc_po.

            ENDIF.

          ENDLOOP.

*---Goods Receipt (MIGO) filled by Vendor
        ELSEIF lt_doc_gr[] IS NOT INITIAL.

          "Check if MIGO is already set in another Process
          SELECT p~guid_header
                 p~doc_number
            INTO TABLE lt_po
            FROM /tcsr/t_po AS p
           INNER JOIN /tcsr/t_act AS a
              ON p~guid_header = a~guid_header
            FOR ALL ENTRIES IN lt_doc_gr
            WHERE p~doc_number      EQ lt_doc_gr-doc_number
              AND p~gjahr           EQ lt_doc_gr-gjahr
              AND a~last_stepstatus NE '102'. "Inutilizado
          IF sy-subrc NE 0.

            "Select MIGO from SAP
            SELECT mblnr
                   mjahr
                   zeile
                   matnr
                   lifnr
                   menge
                   meins
                   waers
                   dmbtr
                   ebeln
                   ebelp
                   sjahr
                   smbln
                   smblp
              INTO TABLE lt_mseg
              FROM mseg
              FOR ALL ENTRIES IN lt_doc_gr
              WHERE mblnr = lt_doc_gr-doc_number AND
                    mjahr = lt_doc_gr-gjahr.
            IF sy-subrc = 0.

              SELECT ekpo~ebeln
                     ekpo~ebelp
                     ekko~bukrs
                     ekpo~werks
                     ekko~loekz
                     ekko~bsart
                     ekko~aedat
                     ekko~ernam
                     ekpo~matnr
                     ekpo~txz01
                     ekpo~menge
                     ekpo~netwr
                     ekpo~j_1bnbm
                     ekko~ekorg
                INTO TABLE lt_ekpo
                FROM ekpo
               INNER JOIN ekko
                  ON ekko~ebeln EQ ekpo~ebeln
               FOR ALL ENTRIES IN lt_mseg
               WHERE ekpo~ebeln = lt_mseg-ebeln
                 AND ekpo~ebelp = lt_mseg-ebelp.
              IF sy-subrc = 0.

                lt_ekpo_fae[] = lt_ekpo[].
                SORT lt_ekpo_fae BY werks.
                DELETE ADJACENT DUPLICATES FROM lt_ekpo_fae COMPARING werks.

                "CHECK: CNPJ Company/Branch x CNPJ NFS-e Tomador
                SELECT werks
                       j_1bbranch
                  INTO TABLE lt_t001w
                  FROM t001w
                  FOR ALL ENTRIES IN lt_ekpo_fae
                 WHERE werks EQ lt_ekpo_fae-werks.
              ENDIF.

            ENDIF.

          ENDIF.

          SORT lt_po BY guid_header.
          SORT lt_mseg BY mblnr mjahr.

          LOOP AT lt_doc_gr INTO lw_doc_gr.
            READ TABLE lt_po INTO lw_po WITH KEY guid_header = me->mw_hd-guid_header.
            IF sy-subrc = 0.
              MESSAGE s354(/tcsr/msg) INTO me->mv_dummy       "Documento de Material & & já associado ao Processo NFS-e GUID &
                                      WITH lw_doc_gr-doc_number lw_doc_gr-gjahr.
              CALL METHOD lo_bal_log->bal_log_msg_add
                EXPORTING
                  iw_log_handle = lw_log_handle.
              CALL METHOD lo_bal_log->bal_db_save
                EXPORTING
                  iw_log_handle = lw_log_handle.
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
              me->mw_act-ref_doc_error = abap_true.
              CONTINUE.
            ENDIF.

            READ TABLE lt_mseg INTO lw_mseg WITH KEY mblnr = lw_doc_gr-doc_number
                                                     mjahr = lw_doc_gr-gjahr
                                                            BINARY SEARCH.
            IF sy-subrc NE 0.
              MESSAGE s352(/tcsr/msg) INTO me->mv_dummy         "Documento de Material & & não existe
                                      WITH lw_doc_gr-doc_number lw_doc_gr-gjahr.
              CALL METHOD lo_bal_log->bal_log_msg_add
                EXPORTING
                  iw_log_handle = lw_log_handle.
              CALL METHOD lo_bal_log->bal_db_save
                EXPORTING
                  iw_log_handle = lw_log_handle.
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
              me->mw_act-ref_doc_error = abap_true.
              CONTINUE.
            ENDIF.

            READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_mseg-ebeln
                                                     ebelp = lw_mseg-ebelp.
            IF sy-subrc = 0.

              IF lw_ekpo-bukrs NE me->mw_act-bukrs.
                MESSAGE s289(/tcsr/msg) INTO me->mv_dummy     "Empresa & da PO & diferente da empresa do CNPJ & informado no XML
                                        WITH lw_ekpo-bukrs lw_doc_po-doc_number me->mw_hd-t_cnpj.
                CALL METHOD lo_bal_log->bal_log_msg_add
                  EXPORTING
                    iw_log_handle = lw_log_handle.
                CALL METHOD lo_bal_log->bal_db_save
                  EXPORTING
                    iw_log_handle = lw_log_handle.
                me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
                me->mw_act-ref_doc_error = abap_true.
                CONTINUE.
              ENDIF.

              READ TABLE me->mt_proc_doc ASSIGNING <fs_proc_doc> WITH KEY ebeln = lw_ekpo-ebeln
                                                                          ebelp = lw_ekpo-ebelp.
              IF sy-subrc NE 0.
                APPEND INITIAL LINE TO me->mt_proc_doc ASSIGNING <fs_proc_doc>.
                <fs_proc_doc>-guid_header = me->mv_guid_header.
                <fs_proc_doc>-ebeln       = lw_ekpo-ebeln.
                <fs_proc_doc>-ebelp       = lw_ekpo-ebelp.
                <fs_proc_doc>-vgabe       = '1'.
                <fs_proc_doc>-doc_number  = lw_mseg-mblnr.
                <fs_proc_doc>-gjahr       = lw_mseg-mjahr.


                mw_po_list_assig-ebeln     = lw_mseg-ebeln.
                mw_po_list_assig-ebelp     = lw_mseg-ebelp.
                mw_po_list_assig-matnr     = lw_mseg-matnr.
                mw_po_list_assig-txz01     = lw_ekpo-txz01.
                mw_po_list_assig-j_1bnbm   = lw_ekpo-j_1bnbm.
                mw_po_list_assig-bukrs     = lw_ekpo-bukrs.
                mw_po_list_assig-ekorg     = lw_ekpo-ekorg.
                mw_po_list_assig-werks     = lw_ekpo-werks.
                mw_po_list_assig-menge     = lw_ekpo-menge.
                mw_po_list_assig-netwr     = lw_ekpo-netwr.
                mw_po_list_assig-aedat     = lw_ekpo-aedat.
                mw_po_list_assig-ernam     = lw_ekpo-ernam.
                mw_po_list_assig-mblnr     = lw_mseg-mblnr.
                mw_po_list_assig-mjahr     = lw_mseg-mjahr.
                mw_po_list_assig-zeile     = lw_mseg-zeile.
                mw_po_list_assig-menge_gr  = lw_mseg-menge.
                mw_po_list_assig-meins_gr  = lw_mseg-meins.
                mw_po_list_assig-dmbtr_gr  = lw_mseg-dmbtr.
                mw_po_list_assig-waers_gr  = lw_mseg-waers.
                mw_po_list_assig-menge_iv  = lw_mseg-menge.
                mw_po_list_assig-dmbtr_iv  = lw_mseg-dmbtr.

                <fs_proc_doc>-menge       = me->mw_po_list_assig-menge_iv.
                <fs_proc_doc>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.

                APPEND mw_po_list_assig TO mt_po_list_assig.
                CLEAR mw_po_list_assig.

              ENDIF.

              COLLECT lw_ekpo INTO lt_proc_po.

            ENDIF.

          ENDLOOP.

        ENDIF.

*---Purchase Order
        IF lt_doc_po[] IS NOT INITIAL.

          SORT lt_return BY ebeln ebelp.
          REFRESH lt_ekpo.

          SELECT ekpo~ebeln
                 ekpo~ebelp
                 ekko~bukrs
                 ekpo~werks
                 ekko~loekz
                 ekko~bsart
                 ekko~aedat
                 ekko~ernam
                 ekpo~matnr
                 ekpo~txz01
                 ekpo~menge
                 ekpo~netwr
                 ekpo~j_1bnbm
                 ekko~ekorg
                 ekpo~packno
            INTO TABLE lt_ekpo
            FROM ekpo
           INNER JOIN ekko
              ON ekko~ebeln EQ ekpo~ebeln
           FOR ALL ENTRIES IN lt_doc_po
           WHERE ekpo~ebeln = lt_doc_po-ebeln
             AND ekpo~loekz EQ ''.

          IF lt_ekpo[] IS NOT INITIAL.

            "Select Sevices Header
            SELECT packno
                   introw
                   sub_packno
              INTO TABLE lt_esll_pckg
              FROM esll
              FOR ALL ENTRIES IN lt_ekpo
              WHERE packno = lt_ekpo-packno
              ORDER BY PRIMARY KEY.

            "Select Services Item
            SELECT packno
                   introw
                   extrow
                   srvpos
                   ktext1
                   sub_packno
                   menge
                   meins
                   netwr
                   vrtkz
                   act_menge
                   tbtwr
              INTO TABLE lt_esll
              FROM esll
              FOR ALL ENTRIES IN lt_esll_pckg
              WHERE packno = lt_esll_pckg-sub_packno
              ORDER BY PRIMARY KEY.

            SORT lt_ekpo BY ebeln ebelp.
            SORT lt_esll_pckg BY packno.
            SORT lt_esll BY packno.

            LOOP AT lt_doc_po INTO lw_doc_po.

              CLEAR lv_error.

              "If PO came in XML (without item)
              IF lw_doc_po-ebelp IS INITIAL.

                READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_doc_po-ebeln
                                                                  BINARY SEARCH.
                IF sy-subrc EQ 0.

                  "Verify if PO has more than one item
                  LOOP AT lt_ekpo TRANSPORTING NO FIELDS WHERE ebeln EQ lw_ekpo-ebeln AND
                                                               ebelp NE lw_ekpo-ebelp.
                    EXIT.
                  ENDLOOP.
                  IF sy-subrc = 0.
                    MESSAGE s290(/tcsr/msg) INTO me->mv_dummy    "PO & tem mais de um item. Associar item manualmente
                                            WITH lw_doc_po-doc_number.
                    CALL METHOD lo_bal_log->bal_log_msg_add
                      EXPORTING
                        iw_log_handle = lw_log_handle.
                    CALL METHOD lo_bal_log->bal_db_save
                      EXPORTING
                        iw_log_handle = lw_log_handle.
                    me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
                    CONTINUE.
                  ELSE.
                    lw_doc_po-ebelp = lw_ekpo-ebelp.
                  ENDIF.
                ENDIF.

              ELSE.
                READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_doc_po-ebeln
                                                         ebelp = lw_doc_po-ebelp
                                                                  BINARY SEARCH.
              ENDIF.

              "Save PO and item to define process
              COLLECT lw_ekpo INTO lt_proc_po.

              REFRESH lt_doc_po_aux.
              APPEND lw_doc_po TO lt_doc_po_aux.
              CALL FUNCTION '/TCSR/F_CHECK_PO'
                EXPORTING
                  it_po     = lt_doc_po_aux
*                 IV_PROCTYP       =
                  iw_hd     = me->mw_hd
                IMPORTING
                  et_return = lt_return.

              LOOP AT lt_return INTO lw_return WHERE ebeln = lw_doc_po-ebeln AND
                                                     ebelp = lw_doc_po-ebelp.
                IF lw_return-type = 'W'.
                  MESSAGE ID '/TCSR/MSG' TYPE 'S' NUMBER lw_return-number WITH lw_return-attr1 lw_return-attr2
                                                                               lw_return-attr3 lw_return-attr4
                                                                               INTO me->mv_dummy.
                  CALL METHOD lo_bal_log->bal_log_msg_add
                    EXPORTING
                      iw_log_handle = lw_log_handle.
                  CALL METHOD lo_bal_log->bal_db_save
                    EXPORTING
                      iw_log_handle = lw_log_handle.
                  me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
                ELSE.
                  MESSAGE ID '/TCSR/MSG' TYPE 'E' NUMBER lw_return-number WITH lw_return-attr1 lw_return-attr2
                                                                               lw_return-attr3 lw_return-attr4
                                                                               INTO me->mv_dummy.
                  CALL METHOD lo_bal_log->bal_log_msg_add
                    EXPORTING
                      iw_log_handle = lw_log_handle.
                  CALL METHOD lo_bal_log->bal_db_save
                    EXPORTING
                      iw_log_handle = lw_log_handle.
                  me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
                  lv_error = abap_true.
                ENDIF.
              ENDLOOP.
              IF lv_error = abap_true.
                CONTINUE.
              ENDIF.

              "Save documents used to define process
              READ TABLE me->mt_proc_doc ASSIGNING <fs_proc_doc> WITH KEY ebeln = lw_ekpo-ebeln
                                                                          ebelp = lw_ekpo-ebelp.
              IF sy-subrc NE 0.
                READ TABLE lt_esll_pckg INTO lw_esll_pckg WITH KEY packno = lw_ekpo-packno
                                                                              BINARY SEARCH.
                READ TABLE lt_esll INTO lw_esll WITH KEY packno = lw_esll_pckg-sub_packno
                                                                           BINARY SEARCH.
                LOOP AT lt_esll INTO lw_esll FROM sy-tabix.
                  IF lw_esll-packno NE lw_esll_pckg-sub_packno.
                    EXIT.
                  ENDIF.

                  APPEND INITIAL LINE TO me->mt_proc_doc ASSIGNING <fs_proc_doc>.
                  <fs_proc_doc>-guid_header = me->mv_guid_header.
                  <fs_proc_doc>-ebeln       = lw_ekpo-ebeln.
                  <fs_proc_doc>-ebelp       = lw_ekpo-ebelp.
                  <fs_proc_doc>-extrow      = lw_esll-extrow.

                  mw_po_list_assig-bukrs          = lw_ekpo-bukrs.
                  mw_po_list_assig-ekorg          = lw_ekpo-ekorg.
                  mw_po_list_assig-ernam          = lw_ekpo-ernam.
                  mw_po_list_assig-aedat          = lw_ekpo-aedat.
                  mw_po_list_assig-ebeln          = lw_ekpo-ebeln.
                  mw_po_list_assig-ebelp          = lw_ekpo-ebelp.
                  mw_po_list_assig-matnr          = lw_ekpo-matnr.
                  mw_po_list_assig-txz01          = lw_ekpo-txz01.
                  mw_po_list_assig-werks          = lw_ekpo-werks.
                  mw_po_list_assig-j_1bnbm        = lw_ekpo-j_1bnbm.
                  mw_po_list_assig-menge          = lw_ekpo-menge.
                  mw_po_list_assig-netwr          = lw_ekpo-netwr.
                  mw_po_list_assig-menge_iv       = lw_ekpo-menge.
                  mw_po_list_assig-dmbtr_iv       = lw_ekpo-netwr.
                  "Service data
                  mw_po_list_assig-introw         = lw_esll-introw.     "Internal item
                  mw_po_list_assig-extrow         = lw_esll-extrow.     "External item
                  mw_po_list_assig-srvpos         = lw_esll-srvpos.     "Service number
                  mw_po_list_assig-ktext1_ses     = lw_esll-ktext1.     "Serv.Text
                  mw_po_list_assig-menge_ses      = lw_esll-menge.      "Qty
                  mw_po_list_assig-meins_ses      = lw_esll-meins.      "Unit
                  mw_po_list_assig-tbtwr_ses      = lw_esll-tbtwr.      "Gross Value
                  mw_po_list_assig-netwr_ses      = lw_esll-netwr.      "Net Value
                  mw_po_list_assig-act_menge_ses  = lw_esll-menge.      "PO: Entered Quantity
                  mw_po_list_assig-distr_ses      = lw_esll-vrtkz.      "Distribution indicator
                  mw_po_list_assig-menge_iv       = lw_esll-menge.
                  mw_po_list_assig-dmbtr_iv       = lw_esll-tbtwr * lw_esll-menge.

                  <fs_proc_doc>-menge       = me->mw_po_list_assig-menge_iv.
                  <fs_proc_doc>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.

                  APPEND mw_po_list_assig TO mt_po_list_assig.

                ENDLOOP.
              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

        "Get groups to define process
        SELECT *
          INTO TABLE me->mt_proc_det
          FROM /tcsr/t_proc_det
          WHERE active EQ abap_true.
        IF sy-subrc = 0.

          "Get fields values expected for each group
          SELECT *
            INTO TABLE me->mt_proc_grp
            FROM /tcsr/t_proc_grp
            FOR ALL ENTRIES IN me->mt_proc_det
            WHERE group_field = me->mt_proc_det-group_field AND
                  active EQ abap_true.


          LOOP AT lt_proc_po INTO lw_proc_po.

            IF lv_param_proctyp IS NOT INITIAL.
              lv_proctyp_old = lv_param_proctyp.
            ENDIF.

            "Define process type according to customizing
            lv_param_proctyp = me->set_proctyp( iv_ebeln = lw_proc_po-ebeln
                                                iv_ebelp = lw_proc_po-ebelp
                                                iv_lifnr = lv_lifnr ).

            "If not found process type, ask for manual definition
            IF lv_param_proctyp IS INITIAL.
              MESSAGE e274(/tcsr/msg) INTO me->mv_dummy "Não foi possível defnir tipo de processo para PO & item &
                                      WITH lw_ekpo-ebeln lw_ekpo-ebelp.
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).

              "If at least one of PO/item defined a different process type, stop process and ask for manual definition
            ELSEIF lv_proctyp_old NE lv_param_proctyp AND lv_proctyp_old IS NOT INITIAL.
              MESSAGE e275(/tcsr/msg) INTO me->mv_dummy."Tipo de processo não é o mesmo para as POs/item informadas
              me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).
              CLEAR lv_param_proctyp.
              EXIT.
            ENDIF.

          ENDLOOP.

        ELSE.
          IF lv_param_proctyp IS INITIAL.
            lv_param_proctyp = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                        iv_name     = 'PROCTYP_DEFAULT' ).
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

    IF lv_param_proctyp IS NOT INITIAL.

      "Set Process (NORMAL or CANCEL)
      me->mw_act-proctyp = lv_param_proctyp.

      "Select Process Flow
      me->select_procflow( ).

      "Get Description of Process
      SELECT SINGLE procdescr
        INTO lv_procdescr
        FROM /tcsr/t_proctypt
        WHERE proctyp = me->mw_act-proctyp
          AND langu = sy-langu.

      MESSAGE e271(/tcsr/msg)     "Processo NFS-e foi determinado com sucesso
         INTO me->mv_dummy.
      "Set Status "SUCCESS"
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).

      me->mt_proc_doc_upd[] = me->mt_proc_doc[].

    ELSE.
      MESSAGE e276(/tcsr/msg) INTO me->mv_dummy. "Não foi possível defnir tipo de processo automaticamente
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-errordet ).

      IF lt_doc_po IS INITIAL.
        me->mw_act-ref_doc_no = abap_true.
      ELSE.
        me->mw_act-ref_doc_error = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_PROCDET1.

    DATA:
      lt_po_fae TYPE TABLE OF /tcsr/t_po,
      lt_return TYPE TABLE OF bapiret2.
    DATA:
      lw_po         TYPE /tcsr/t_po,
      lw_log_handle TYPE balloghndl.
    DATA:
      lv_param_proctyp TYPE /tcsr/t_proctyp-proctyp,
      lv_proctyp_old   TYPE /tcsr/t_proctyp-proctyp,
      lv_procdescr     TYPE /tcsr/t_proctypt-procdescr.
    FIELD-SYMBOLS:
      <fs_return> TYPE bapiret2.
    DATA:
      lo_param   TYPE REF TO /tcsr/c_param,
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.

    CREATE OBJECT lo_param.

    IF mw_xml_rpsx-inf_rps-status = '2'.
      lv_param_proctyp = 'PROCCANC'.
    ELSE.

      IF me->mt_po[] IS INITIAL.
        MESSAGE e278(/tcsr/msg) "No document assigned to define process type automatically.
           INTO me->mv_dummy.
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
        EXIT.
      ENDIF.

      "Get groups to define process
      SELECT *
        INTO TABLE me->mt_proc_det
        FROM /tcsr/t_proc_det
        WHERE active EQ abap_true.
      IF sy-subrc = 0.

        "Get fields values expected for each group
        SELECT *
          INTO TABLE me->mt_proc_grp
          FROM /tcsr/t_proc_grp
          FOR ALL ENTRIES IN me->mt_proc_det
          WHERE group_field = me->mt_proc_det-group_field AND
                active EQ abap_true.

        lt_po_fae[] = me->mt_po[].
        SORT lt_po_fae BY ebeln ebelp.
        DELETE ADJACENT DUPLICATES FROM lt_po_fae COMPARING ebeln ebelp.

        LOOP AT lt_po_fae INTO lw_po.

          IF lv_param_proctyp IS NOT INITIAL.
            lv_proctyp_old = lv_param_proctyp.
          ENDIF.

          "Define process type according to customizing
          lv_param_proctyp = me->set_proctyp( iv_ebeln = lw_po-ebeln
                                              iv_ebelp = lw_po-ebelp
                                              iv_lifnr = me->mw_act-lifnr ).

          IF lv_param_proctyp IS INITIAL.
            "It was not possible to define process type for PO & item &
            APPEND INITIAL LINE TO lt_return ASSIGNING <fs_return>.
            <fs_return>-type        = 'E'.
            <fs_return>-id          = /tcsr/c_constants=>c_msg_class.
            <fs_return>-number      = '274'.
            <fs_return>-message_v1  = me->mw_po-ebeln.
            <fs_return>-message_v2  = me->mw_po-ebeln.

            "If at least one of PO/item defined a different process type, stop process and ask for manual definition
          ELSEIF lv_proctyp_old NE lv_param_proctyp AND lv_proctyp_old IS NOT INITIAL.
            "Process type is not the same for the PO/item informed.
            APPEND INITIAL LINE TO lt_return ASSIGNING <fs_return>.
            <fs_return>-type        = 'E'.
            <fs_return>-id          = /tcsr/c_constants=>c_msg_class.
            <fs_return>-number      = '275'.

            CLEAR lv_param_proctyp.
            EXIT.
          ENDIF.

        ENDLOOP.

      ELSE.
        IF lv_param_proctyp IS INITIAL.
          lv_param_proctyp = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                                      iv_name     = 'PROCTYP_NORMAL' ).
        ENDIF.
      ENDIF.

    ENDIF.

    "Process Defined
    IF lv_param_proctyp IS NOT INITIAL.

      "Fill internal table mt_po_list_assig to be used in the next step automatically
      me->fill_po_list_assig( ).

      "Set Process (NORMAL or CANCEL)
      me->mw_act-proctyp = lv_param_proctyp.

      "Select Process Flow
      me->select_procflow( ).

      "Get Description of Process
      SELECT SINGLE procdescr
        INTO lv_procdescr
        FROM /tcsr/t_proctypt
        WHERE proctyp = me->mw_act-proctyp
          AND langu = sy-langu.

      "NFS-e Process was successfully determined
      MESSAGE s271(/tcsr/msg) INTO me->mv_dummy.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).

    ELSE.

      IF lt_return[] IS NOT INITIAL.

        "===================================================================
        " CHECK: Return Error
        "===================================================================

        CREATE OBJECT lo_bal_log.

        CALL METHOD lo_bal_log->bal_log_create
          EXPORTING
            iv_guid_header = me->mw_hd-guid_header
          IMPORTING
            ew_log_handle  = lw_log_handle.

        "--------------------------------------------
        " SAVE: Return Error Messages
        "--------------------------------------------
        LOOP AT lt_return ASSIGNING <fs_return>.
          MESSAGE ID <fs_return>-id
             TYPE <fs_return>-type
           NUMBER <fs_return>-number
             WITH <fs_return>-message_v1
                  <fs_return>-message_v2
                  <fs_return>-message_v3
                  <fs_return>-message_v4
             INTO me->mv_dummy.
          CALL METHOD lo_bal_log->bal_log_msg_add
            EXPORTING
              iw_log_handle = lw_log_handle.
        ENDLOOP.

        CALL METHOD lo_bal_log->bal_db_save
          EXPORTING
            iw_log_handle = lw_log_handle.

      ENDIF.

      "It was not possible to define process type automatically.
      MESSAGE e276(/tcsr/msg) INTO me->mv_dummy.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
    ENDIF.


  ENDMETHOD.


  METHOD EXECUTE_SAVE_POASSIGN.

    DATA:
      lw_po_list_unass TYPE /tcsr/s_po_list,
      lw_po_list_assig TYPE /tcsr/s_po_list.
    DATA:
      lv_doc_number TYPE /tcsr/t_proc_doc-doc_number.
    FIELD-SYMBOLS:
      <fs_po>     TYPE /tcsr/t_po,
      <fs_po_del> TYPE /tcsr/t_po.

    "--------------------------------------------
    " CHECK: IF any line was UNASSIGNED
    "--------------------------------------------
    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass WHERE ebeln IS NOT INITIAL.

      CLEAR lv_doc_number.

      "Check PO is assigned
      READ TABLE me->mt_po_list_assig INTO lw_po_list_assig WITH KEY ebeln  = lw_po_list_unass-ebeln
                                                                     ebelp  = lw_po_list_unass-ebelp
                                                                     extrow = lw_po_list_unass-extrow.
      IF sy-subrc NE 0.

        MESSAGE s277(/tcsr/msg) INTO me->mv_dummy. "Documento(s) desassociado(s).
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-unassigned ).

        "Append PO/ITEM UNASSIGNED
        APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
        <fs_po_del>-guid_header = me->mv_guid_header.
        <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
        <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
        <fs_po_del>-extrow      = lw_po_list_unass-extrow.

      ENDIF.
      CLEAR: me->mw_act-tolerance.

    ENDLOOP.

    "--------------------------------------------
    " CHECK documents assigned at monitor
    "--------------------------------------------
    REFRESH me->mt_po.
    LOOP AT me->mt_po_list_assig INTO me->mw_po_list_assig.

      IF me->mw_po_list_assig-ebeln IS NOT INITIAL AND
         me->mw_po_list_assig-ebelp IS NOT INITIAL.

        APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
        <fs_po>-guid_header = me->mv_guid_header.
        <fs_po>-ebeln       = me->mw_po_list_assig-ebeln.
        <fs_po>-ebelp       = me->mw_po_list_assig-ebelp.
        <fs_po>-extrow      = me->mw_po_list_assig-extrow.
        <fs_po>-menge       = me->mw_po_list_assig-menge_iv.
        <fs_po>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.

        MESSAGE s341(/tcsr/msg) INTO me->mv_dummy  "Purchase Order & assigned successfully
                                WITH <fs_po>-ebeln.
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-success ).

        "Update Active Table
        me->mw_act-tolerance      = me->mv_tolerance.       "Save Tolerance Value
        me->mw_act-tolerance_tax  = me->mv_tolerance_tax.   "Save Tolerance Tax

      ENDIF.

    ENDLOOP.

    me->mt_po_upd[] = me->mt_po[].

  ENDMETHOD.


  METHOD EXECUTE_SAVE_PROCDET.

    DATA:
      lw_po_list_unass TYPE /tcsr/s_po_list,
      lw_po_list_assig TYPE /tcsr/s_po_list.
    DATA:
      lv_doc_number TYPE /tcsr/t_proc_doc-doc_number.
    FIELD-SYMBOLS:
      <fs_po> TYPE /tcsr/t_po,
      <fs_po_del>   TYPE /tcsr/t_po.

    "--------------------------------------------
    " CHECK: IF any line was UNASSIGNED
    "--------------------------------------------
    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass WHERE ebeln IS NOT INITIAL.

      CLEAR lv_doc_number.

      "Check PO is assigned
      READ TABLE me->mt_po_list_assig INTO lw_po_list_assig WITH KEY ebeln  = lw_po_list_unass-ebeln
                                                                     ebelp  = lw_po_list_unass-ebelp
                                                                     lblni  = lw_po_list_unass-lblni
                                                                     extrow = lw_po_list_unass-extrow
                                                                     mblnr  = lw_po_list_unass-mblnr
                                                                     mjahr  = lw_po_list_unass-mjahr.
      IF sy-subrc NE 0.

        MESSAGE s277(/tcsr/msg) INTO me->mv_dummy. "Documento(s) desassociado(s).
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-unassigned ).

        IF lw_po_list_unass-lblni IS NOT INITIAL.
          lv_doc_number = lw_po_list_unass-lblni.
        ELSEIF lw_po_list_unass-mblnr IS NOT INITIAL.
          lv_doc_number = lw_po_list_unass-mblnr.
        ENDIF.

        "Append PO/ITEM/DOC UNASSIGNED
        APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
        <fs_po_del>-guid_header = me->mv_guid_header.
        <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
        <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
        <fs_po_del>-doc_number  = lv_doc_number.
        <fs_po_del>-gjahr       = lw_po_list_unass-mjahr.
        <fs_po_del>-extrow      = lw_po_list_unass-extrow.

        READ TABLE me->mt_po TRANSPORTING NO FIELDS WITH KEY ebeln       = lw_po_list_unass-ebeln
                                                             ebelp       = lw_po_list_unass-ebelp
                                                             doc_number  = lv_doc_number
                                                             gjahr       = lw_po_list_unass-mjahr
                                                             extrow      = lw_po_list_unass-extrow.
        IF sy-subrc = 0.
          DELETE me->mt_po INDEX sy-tabix.
        ENDIF.
      ENDIF.
      CLEAR: me->mw_act-tolerance.

    ENDLOOP.

    "--------------------------------------------
    " CHECK documents assigned at monitor
    "--------------------------------------------
    LOOP AT me->mt_po_list_assig INTO me->mw_po_list_assig.

      IF me->mw_po_list_assig-ebeln IS NOT INITIAL AND
         me->mw_po_list_assig-ebelp IS NOT INITIAL.

        APPEND INITIAL LINE TO me->mt_po ASSIGNING <fs_po>.
        <fs_po>-guid_header = me->mv_guid_header.
        <fs_po>-ebeln       = me->mw_po_list_assig-ebeln.
        <fs_po>-ebelp       = me->mw_po_list_assig-ebelp.
        <fs_po>-extrow      = me->mw_po_list_assig-extrow.
        <fs_po>-menge       = me->mw_po_list_assig-menge_iv.
        <fs_po>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.

        "Assigned SES
        IF me->mw_po_list_assig-lblni IS NOT INITIAL.
          <fs_po>-vgabe      = '9'.
          <fs_po>-doc_number = me->mw_po_list_assig-lblni.

          "Assigned MIGO
        ELSEIF me->mw_po_list_assig-mblnr IS NOT INITIAL.
          <fs_po>-vgabe      = '1'.
          <fs_po>-doc_number = me->mw_po_list_assig-mblnr.
          <fs_po>-gjahr      = me->mw_po_list_assig-mjahr.
        ENDIF.

        MESSAGE s279(/tcsr/msg) INTO me->mv_dummy.  "Assigned documents successfully
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-success ).

        "Update Active Table
        me->mw_act-tolerance      = me->mv_tolerance.       "Save Tolerance Value
        me->mw_act-tolerance_tax  = me->mv_tolerance_tax.   "Save Tolerance Tax

      ENDIF.

    ENDLOOP.

    me->mt_po_upd[] = me->mt_po[].

  ENDMETHOD.


  METHOD EXECUTE_SAVE_PROCESS.

    DATA:
      lt_proc_doc TYPE TABLE OF /tcsr/t_proc_doc.
    DATA:
      lw_po_list_unass TYPE /tcsr/s_po_list,
      lw_po_list_assig TYPE /tcsr/s_po_list.
    FIELD-SYMBOLS:
      <fs_proc_doc> TYPE /tcsr/t_proc_doc,
      <fs_po_del>   TYPE /tcsr/t_po.
    DATA: lv_doc_number TYPE /tcsr/t_proc_doc-doc_number.


    "--------------------------------------------
    " CHECK: IF any line was UNASSIGNED
    "--------------------------------------------
    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass WHERE ebeln IS NOT INITIAL.

      CLEAR lv_doc_number.

      "Check PO is assigned
      READ TABLE me->mt_po_list_assig INTO lw_po_list_assig WITH KEY ebeln  = lw_po_list_unass-ebeln
                                                                     ebelp  = lw_po_list_unass-ebelp
                                                                     lblni  = lw_po_list_unass-lblni
                                                                     extrow = lw_po_list_unass-extrow
                                                                     mblnr  = lw_po_list_unass-mblnr
                                                                     mjahr  = lw_po_list_unass-mjahr.
      IF sy-subrc NE 0.

*        "Unassigned SES
*        IF lw_po_list_unass-lblni IS NOT INITIAL.
*          lv_doc_number = lw_po_list_unass-lblni.
*          MESSAGE s301(/tcsr/msg) INTO me->mv_dummy "Desassociada Folha Registro de Serviço &
*                                  WITH lw_po_list_unass-lblni.
*          "Unassigned MIGO
*        ELSEIF lw_po_list_unass-mblnr IS NOT INITIAL.
*          lv_doc_number = lw_po_list_unass-mblnr.
*          MESSAGE s359(/tcsr/msg) INTO me->mv_dummy "Desassociado Documento de Material & &
*                                  WITH lw_po_list_unass-mblnr.
*          "Unassigned PO
*        ELSE.
*          MESSAGE s311(/tcsr/msg) INTO me->mv_dummy "Desassociado Pedido & e Item &
*                                  WITH lw_po_list_unass-ebeln lw_po_list_unass-ebelp.
*        ENDIF.
        MESSAGE s277(/tcsr/msg) INTO me->mv_dummy. "Documento(s) desassociado(s).
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-unassigned ).

        IF lw_po_list_unass-lblni IS NOT INITIAL.
          lv_doc_number = lw_po_list_unass-lblni.
        ELSEIF lw_po_list_unass-mblnr IS NOT INITIAL.
          lv_doc_number = lw_po_list_unass-mblnr.
        ENDIF.

        "Append PO/ITEM/DOC UNASSIGNED
        APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
        <fs_po_del>-guid_header = me->mv_guid_header.
        <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
        <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
        <fs_po_del>-doc_number  = lv_doc_number.
        <fs_po_del>-gjahr       = lw_po_list_unass-mjahr.
        <fs_po_del>-extrow      = lw_po_list_unass-extrow.

      ENDIF.
      CLEAR: me->mw_act-tolerance.

    ENDLOOP.

    "--------------------------------------------
    " CHECK documents assigned at monitor
    "--------------------------------------------
    LOOP AT me->mt_po_list_assig INTO me->mw_po_list_assig.

      IF me->mw_po_list_assig-ebeln IS NOT INITIAL AND
         me->mw_po_list_assig-ebelp IS NOT INITIAL.

        APPEND INITIAL LINE TO me->mt_proc_doc ASSIGNING <fs_proc_doc>.
        <fs_proc_doc>-guid_header = me->mv_guid_header.
        <fs_proc_doc>-ebeln       = me->mw_po_list_assig-ebeln.
        <fs_proc_doc>-ebelp       = me->mw_po_list_assig-ebelp.
        <fs_proc_doc>-extrow      = me->mw_po_list_assig-extrow.
        <fs_proc_doc>-menge       = me->mw_po_list_assig-menge_iv.
        <fs_proc_doc>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.

        "Assigned SES
        IF me->mw_po_list_assig-lblni IS NOT INITIAL.
          <fs_proc_doc>-vgabe      = '9'.
          <fs_proc_doc>-doc_number = me->mw_po_list_assig-lblni.

*          MESSAGE s303(/tcsr/msg) INTO me->mv_dummy       "Associada Folha de Serviço &
*                                  WITH me->mw_po_list_assig-lblni.

          "Assigned MIGO
        ELSEIF me->mw_po_list_assig-mblnr IS NOT INITIAL.
          <fs_proc_doc>-vgabe      = '1'.
          <fs_proc_doc>-doc_number = me->mw_po_list_assig-mblnr.
          <fs_proc_doc>-gjahr      = me->mw_po_list_assig-mjahr.

*          MESSAGE s357(/tcsr/msg) INTO me->mv_dummy       "Associado Documento de Material & &
*                                  WITH lw_po_list_assig-ebeln lw_po_list_assig-ebelp.

        ELSE.
*          MESSAGE s312(/tcsr/msg) INTO me->mv_dummy       "Associado Pedido de Compras & e item
*                                  WITH lw_po_list_assig-ebeln lw_po_list_assig-ebelp.

        ENDIF.
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-success ).

        "Update Active Table
        me->mw_act-tolerance      = me->mv_tolerance.       "Save Tolerance Value
        me->mw_act-tolerance_tax  = me->mv_tolerance_tax.   "Save Tolerance Tax

      ENDIF.

    ENDLOOP.

    "IF PO and item are assigned, get Process
    IF me->mt_proc_doc[] IS NOT INITIAL.
      me->execute_procdet( ).
    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_STOP_PROCESS.

    "Set Guid Cancel.
    me->mw_act-guid_cancel = me->mv_guid_cancel.

    MESSAGE s451(/tcsr/msg)   "Recebida NFS-e de Cancelamento GUID - &
      INTO me->mv_dummy WITH me->mv_guid_cancel.
    me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-stopped ).

  ENDMETHOD.


  METHOD EXECUTE_STOP_PROCESS_CONTINUE.

    DATA:
      lt_hd_orig  TYPE TABLE OF /tcsr/t_hd,
      lt_act_orig TYPE TABLE OF /tcsr/t_act.
    DATA:
      lv_cancel_error TYPE char1.
    FIELD-SYMBOLS:
      <fs_act_orig> TYPE /tcsr/t_act.
*------------------------------------------------

    "Get Original NFS-e
    SELECT *
      INTO TABLE lt_hd_orig
      FROM /tcsr/t_hd
      WHERE nfse_numero = me->mw_hd-nfse_numero
        AND nfse_year   = me->mw_hd-nfse_year
        AND p_cpf       = me->mw_hd-p_cpf
        AND p_cnpj      = me->mw_hd-p_cnpj
      ORDER BY PRIMARY KEY.

    "Delete Guid of NFS-e canceled
    DELETE lt_hd_orig WHERE guid_header EQ me->mw_hd-guid_header.

    IF lt_hd_orig[] IS NOT INITIAL.

      "Select Status
      SELECT *
        INTO TABLE lt_act_orig
        FROM /tcsr/t_act
        FOR ALL ENTRIES IN lt_hd_orig
        WHERE guid_header = lt_hd_orig-guid_header
        ORDER BY PRIMARY KEY.

      DELETE lt_act_orig WHERE last_stepstatus EQ '102'. "NFS-e Process Canceled

      "IF found Active NFS-e Process -
      IF lt_act_orig[] IS NOT INITIAL.

        "UPDATE all NFS-e to STATUS 89 - NFS-e Canceled
        LOOP AT lt_act_orig ASSIGNING <fs_act_orig>.

          CALL FUNCTION 'ENQUEUE_/TCSR/ENQ_GUID'
            EXPORTING
              mode_/tcsr/s_enq_guid = 'E'
              mandt                 = sy-mandt
              guid_header           = <fs_act_orig>-guid_header
              _scope                = '3'
            EXCEPTIONS
              foreign_lock          = 1
              system_failure        = 2
              OTHERS                = 3.
          IF sy-subrc NE 0.

            lv_cancel_error = 'X'.

            MESSAGE e452(/tcsr/msg) "Processo NFS-e Bloqueado GUID: &
               INTO me->mv_dummy WITH <fs_act_orig>-guid_header.
            me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-error ).

          ELSE.

            "Execute CANCEL of Original NFS-e Process
            CALL FUNCTION '/TCSR/F_PROCESS_STEPS'
              EXPORTING
                iv_guid_header = <fs_act_orig>-guid_header
                iv_action      = /tcsr/c_constants=>mc_action-stopproc
                iv_guid_cancel = me->mw_hd-guid_header
                iv_call_step   = <fs_act_orig>-last_step.

            MESSAGE e453(/tcsr/msg) "Cancelado Processo NFS-e GUID: &
               INTO me->mv_dummy WITH <fs_act_orig>-guid_header .
            me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-success ).

          ENDIF.

        ENDLOOP.

        "Check if ALL NFS-e Process was Canceled
        IF lv_cancel_error IS INITIAL.
          MESSAGE e454(/tcsr/msg)  "Processos NFS-e cancelados
             INTO me->mv_dummy.
          me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-canceled ).
        ENDIF.

      ELSE.

        MESSAGE e456(/tcsr/msg) "NFS-e de cancelamento recebida com sucesso
          INTO me->mv_dummy.
        "Set Status "SUCESS"
        me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-canceled ).

      ENDIF.

    ELSE.

      MESSAGE e456(/tcsr/msg) "NFS-e de cancelamento recebida com sucesso
         INTO me->mv_dummy.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-canceled ).

    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_UNDO_PROCDET.

    DATA:
      lt_sta TYPE TABLE OF /tcsr/t_sta.
    DATA:
      lw_sta TYPE /tcsr/t_sta.
    DATA:
      lv_tabix TYPE sy-tabix.
    FIELD-SYMBOLS:
      <fs_sta>          TYPE /tcsr/t_sta,
      <fs_sta_del>      TYPE /tcsr/t_sta,
      <fs_po>           TYPE /tcsr/t_po,
      <fs_po_del>       TYPE /tcsr/t_po,
      <fs_proc_doc>     TYPE /tcsr/t_proc_doc,
      <fs_proc_doc_del> TYPE /tcsr/t_proc_doc.
*------------------------------------------------

*    lt_sta[] = me->mt_sta[].
*    SORT lt_sta BY stepcount ASCENDING.

    READ TABLE me->mt_sta INTO me->mw_sta WITH KEY step = 'PROCDET'.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix + 1.
    ENDIF.

    "FILL next step to be deleted
    LOOP AT me->mt_sta INTO lw_sta FROM lv_tabix.
      DELETE me->mt_sta INDEX sy-tabix.

      APPEND INITIAL LINE TO me->mt_sta_del ASSIGNING <fs_sta_del>.
      <fs_sta_del> = lw_sta.
    ENDLOOP.

    LOOP AT me->mt_po ASSIGNING <fs_po>.
      APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
      <fs_po_del> = <fs_po>.
    ENDLOOP.

    LOOP AT me->mt_proc_doc ASSIGNING <fs_proc_doc>.
      APPEND INITIAL LINE TO me->mt_proc_doc_del ASSIGNING <fs_proc_doc_del>.
      <fs_proc_doc_del> = <fs_proc_doc>.
    ENDLOOP.

    "Set Default Process
    me->mw_act-proctyp = 'DEFAULT'.

    MESSAGE s203(/tcsr/msg) INTO me->mv_dummy. "Etapa reiniciada
    me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-undone ).

  ENDMETHOD.


  METHOD EXECUTE_UNDO_STEP.

    MESSAGE s203(/tcsr/msg) INTO me->mv_dummy. "Etapa reiniciada
    me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-undone ).

  ENDMETHOD.


  METHOD EXECUTE_VALIDATE_CONTINUE.

    IF me->mv_manual IS NOT INITIAL.
      me->mw_act-inss_base    = me->mv_inss_base.
      me->mw_act-ir_base      = me->mv_ir_base.
      me->mw_act-approve_diff = me->mv_approved_diff.
    ENDIF.

    me->mw_act_upd = me->mw_act.

    IF me->mv_approved_diff IS NOT INITIAL.
      me->mw_approval-guid_header   = me->mw_act-guid_header.
      me->mw_approval-approved_date = sy-datum.
      me->mw_approval-approved_time = sy-uzeit.
      me->mw_approval-approved_by   = sy-uname.
      me->mw_approval-proctyp       = me->mw_act-proctyp.
      me->mw_approval-last_step     = me->mw_act-last_step.
    ENDIF.

  ENDMETHOD.


  METHOD EXECUTE_VALIDATE_SAVE.

    DATA:
      lw_po_list_unass TYPE /tcsr/s_po_list.
    FIELD-SYMBOLS:
      <fs_po_upd> TYPE /tcsr/t_po,
      <fs_po_del> TYPE /tcsr/t_po.

    me->mw_act-inss_base    = me->mv_inss_base.
    me->mw_act-ir_base      = me->mv_ir_base.
    me->mw_act-approve_diff = me->mv_approved_diff.

    me->mw_act_upd = me->mw_act.

    IF me->mv_approved_diff IS NOT INITIAL.
      me->mw_approval-guid_header   = me->mw_act-guid_header.
      me->mw_approval-approved_date = sy-datum.
      me->mw_approval-approved_time = sy-uzeit.
      me->mw_approval-approved_by   = sy-uname.
      me->mw_approval-proctyp       = me->mw_act-proctyp.
      me->mw_approval-last_step     = me->mw_act-last_step.
    ENDIF.

    me->mt_po[] = me->mt_po_upd[].

    CHECK me->mw_act-proctyp = 'PROCIV'.

    LOOP AT me->mt_po_list_unass INTO lw_po_list_unass.
      "Update PO table to remove register
      APPEND INITIAL LINE TO me->mt_po_del ASSIGNING <fs_po_del>.
      <fs_po_del>-guid_header = me->mv_guid_header.
      <fs_po_del>-ebeln       = lw_po_list_unass-ebeln.
      <fs_po_del>-ebelp       = lw_po_list_unass-ebelp.
      <fs_po_del>-extrow      = lw_po_list_unass-extrow.
    ENDLOOP.

    LOOP AT me->mt_po_list_assig INTO me->mw_po_list_assig.
      "Update PO table to insert register
      APPEND INITIAL LINE TO me->mt_po_upd ASSIGNING <fs_po_upd>.
      <fs_po_upd>-guid_header = me->mv_guid_header.
      <fs_po_upd>-ebeln       = me->mw_po_list_assig-ebeln.
      <fs_po_upd>-ebelp       = me->mw_po_list_assig-ebelp.
      <fs_po_upd>-extrow      = me->mw_po_list_assig-extrow.
      <fs_po_upd>-menge       = me->mw_po_list_assig-menge_iv.
      <fs_po_upd>-dmbtr       = me->mw_po_list_assig-dmbtr_iv.
    ENDLOOP.

    me->mt_po[] = me->mt_po_upd[].

  ENDMETHOD.


  METHOD EXECUTE_VALIDATE_UNDO.

    CLEAR: me->mw_act-inss_base.
    me->mw_act-approve_diff = me->mv_approved_diff.

    me->mw_act_upd = me->mw_act.

  ENDMETHOD.


  METHOD FILL_PO_LIST_ASSIG.

    TYPES: BEGIN OF ty_essr,
             lblni  TYPE essr-lblni,
             packno TYPE essr-packno,
             aedat  TYPE essr-aedat,
           END OF ty_essr,

           BEGIN OF ty_eslh,
             packno TYPE eslh-packno,
             waers  TYPE eslh-waers,
           END OF ty_eslh,

           BEGIN OF ty_esll_pckg,
             packno     TYPE esll-packno,
             introw     TYPE esll-introw,
             sub_packno TYPE esll-sub_packno,
           END OF ty_esll_pckg,

           BEGIN OF ty_esll,
             packno     TYPE esll-packno,
             introw     TYPE esll-introw,
             extrow     TYPE esll-extrow,
             srvpos     TYPE esll-srvpos,
             ktext1     TYPE esll-ktext1,
             sub_packno TYPE esll-sub_packno,
             menge      TYPE esll-menge,
             meins      TYPE esll-meins,
             netwr      TYPE esll-netwr,
             vrtkz      TYPE esll-vrtkz,
             act_menge  TYPE esll-act_menge,
             tbtwr      TYPE esll-tbtwr,
           END OF ty_esll,

           BEGIN OF ty_ekpo,
             ebeln   TYPE ekpo-ebeln,
             ebelp   TYPE ekpo-ebelp,
             bukrs   TYPE ekko-bukrs,
             werks   TYPE ekpo-werks,
             loekz   TYPE ekko-loekz,
             bsart   TYPE ekko-bsart,
             aedat   TYPE ekko-aedat,
             ernam   TYPE ekko-ernam,
             matnr   TYPE ekpo-matnr,
             txz01   TYPE ekpo-txz01,
             menge   TYPE ekpo-menge,
             netwr   TYPE ekpo-netwr,
             j_1bnbm TYPE ekpo-j_1bnbm,
             ekorg   TYPE ekko-ekorg,
             packno  TYPE ekpo-packno,
           END OF ty_ekpo,

           BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             mjahr TYPE mseg-mjahr,
             zeile TYPE mseg-zeile,
             matnr TYPE mseg-matnr,
             lifnr TYPE mseg-lifnr,
             menge TYPE mseg-menge,
             meins TYPE mseg-meins,
             waers TYPE mseg-waers,
             dmbtr TYPE mseg-dmbtr,
             ebeln TYPE mseg-ebeln,
             ebelp TYPE mseg-ebelp,
             sjahr TYPE mseg-sjahr,
             smbln TYPE mseg-smbln,
             smblp TYPE mseg-smblp,
           END OF ty_mseg.

    DATA:
      lt_po_fae    TYPE TABLE OF /tcsr/t_po,
      lt_essr      TYPE TABLE OF ty_essr,
      lt_eslh      TYPE TABLE OF ty_eslh,
      lt_esll_pckg TYPE TABLE OF ty_esll_pckg,
      lt_esll      TYPE TABLE OF ty_esll,
      lt_ekpo      TYPE TABLE OF ty_ekpo,
      lt_mseg      TYPE TABLE OF ty_mseg,
      lt_return    TYPE TABLE OF /tcsr/s_return_po_check.
    DATA:
      lw_essr       TYPE ty_essr,
      lw_eslh       TYPE ty_eslh,
      lw_esll       TYPE ty_esll,
      lw_esll_pckg  TYPE ty_esll_pckg,
      lw_po         TYPE /tcsr/t_po,
      lw_ekpo       TYPE ty_ekpo,
      lw_mseg       TYPE ty_mseg,
      lw_return     TYPE /tcsr/s_return_po_check,
      lw_log_handle TYPE balloghndl.
    FIELD-SYMBOLS:
      <fs_po_list> TYPE /tcsr/s_po_list.

    REFRESH me->mt_po_list_assig.

    "SES - Get data
    lt_po_fae[] = me->mt_po[].
    DELETE lt_po_fae WHERE vgabe NE '9'.
    IF lt_po_fae[] IS NOT INITIAL.
      SELECT lblni
             packno
             aedat
        INTO TABLE lt_essr
        FROM essr
        FOR ALL ENTRIES IN lt_po_fae
        WHERE lblni = lt_po_fae-doc_number.
      IF sy-subrc = 0.
        SORT lt_essr BY lblni.

        "Select header of Service
        SELECT packno
               waers
          INTO TABLE lt_eslh
          FROM eslh
          FOR ALL ENTRIES IN lt_essr
          WHERE packno = lt_essr-packno.

        "Select Sevices Header
        SELECT packno
               introw
               sub_packno
          INTO TABLE lt_esll_pckg
          FROM esll
          FOR ALL ENTRIES IN lt_essr
          WHERE packno = lt_essr-packno.
        IF sy-subrc EQ 0.

          "Select Services Item
          SELECT packno
                 introw
                 extrow
                 srvpos
                 ktext1
                 sub_packno
                 menge
                 meins
                 netwr
                 vrtkz
                 act_menge
                 tbtwr
            INTO TABLE lt_esll
            FROM esll
            FOR ALL ENTRIES IN lt_esll_pckg
            WHERE packno = lt_esll_pckg-sub_packno.
        ENDIF.
      ENDIF.
    ENDIF.


    "MIGO - Get data
    lt_po_fae[] = me->mt_po[].
    DELETE lt_po_fae WHERE vgabe NE '1'.
    IF lt_po_fae[] IS NOT INITIAL.
      SELECT mblnr
             mjahr
             zeile
             matnr
             lifnr
             menge
             meins
             waers
             dmbtr
             ebeln
             ebelp
             sjahr
             smbln
             smblp
        INTO TABLE lt_mseg
        FROM mseg
        FOR ALL ENTRIES IN lt_po_fae
        WHERE mblnr = lt_po_fae-doc_number
          AND mjahr = lt_po_fae-gjahr.
      SORT lt_mseg BY mblnr mjahr.
    ENDIF.


    "PO - Get data
    lt_po_fae[] = me->mt_po[].
    DELETE lt_po_fae WHERE ebeln IS INITIAL.
    IF lt_po_fae[] IS NOT INITIAL.

      SELECT ekpo~ebeln
             ekpo~ebelp
             ekko~bukrs
             ekpo~werks
             ekko~loekz
             ekko~bsart
             ekko~aedat
             ekko~ernam
             ekpo~matnr
             ekpo~txz01
             ekpo~menge
             ekpo~netwr
             ekpo~j_1bnbm
             ekko~ekorg
             ekpo~packno
        INTO TABLE lt_ekpo
        FROM ekpo
        INNER JOIN ekko
          ON ekko~ebeln EQ ekpo~ebeln
        FOR ALL ENTRIES IN lt_po_fae
        WHERE ekpo~ebeln = lt_po_fae-ebeln
          AND ekpo~ebelp = lt_po_fae-ebelp.

      IF lt_ekpo[] IS NOT INITIAL.

        "Select Sevices Header
        SELECT packno
               introw
               sub_packno
          APPENDING TABLE lt_esll_pckg
          FROM esll
          FOR ALL ENTRIES IN lt_ekpo
          WHERE packno = lt_ekpo-packno.

        "Select Services Item
        SELECT packno
               introw
               extrow
               srvpos
               ktext1
               sub_packno
               menge
               meins
               netwr
               vrtkz
               act_menge
               tbtwr
          APPENDING TABLE lt_esll
          FROM esll
          FOR ALL ENTRIES IN lt_esll_pckg
          WHERE packno = lt_esll_pckg-sub_packno.
      ENDIF.
    ENDIF.

    SORT lt_ekpo BY ebeln ebelp.
    SORT lt_esll_pckg BY packno.
    SORT lt_esll BY packno extrow.

    LOOP AT me->mt_po INTO lw_po.

      CLEAR lw_ekpo.
      READ TABLE lt_ekpo INTO lw_ekpo WITH KEY ebeln = lw_po-ebeln
                                               ebelp = lw_po-ebelp
                                                     BINARY SEARCH.

      APPEND INITIAL LINE TO me->mt_po_list_assig ASSIGNING <fs_po_list>.
      <fs_po_list>-bukrs   = lw_ekpo-bukrs.
      <fs_po_list>-ekorg   = lw_ekpo-ekorg.
      <fs_po_list>-ernam   = lw_ekpo-ernam.
      <fs_po_list>-aedat   = lw_ekpo-aedat.
      <fs_po_list>-ebeln   = lw_ekpo-ebeln.
      <fs_po_list>-ebelp   = lw_ekpo-ebelp.
      <fs_po_list>-matnr   = lw_ekpo-matnr.
      <fs_po_list>-txz01   = lw_ekpo-txz01.
      <fs_po_list>-werks   = lw_ekpo-werks.
      <fs_po_list>-j_1bnbm = lw_ekpo-j_1bnbm.
      <fs_po_list>-menge   = lw_ekpo-menge.
      <fs_po_list>-netwr   = lw_ekpo-netwr.

      <fs_po_list>-menge_iv       = lw_po-menge.
      <fs_po_list>-dmbtr_iv       = lw_po-dmbtr.


      IF lw_po-vgabe EQ '9'.
"*---> 01/07/2023 - Migração S4 - LO
        SORT lt_essr by lblni.
"*---> 01/07/2023 - Migração S4 - LO
        READ TABLE lt_essr INTO lw_essr WITH KEY lblni = lw_po-doc_number
                                                                BINARY SEARCH.
        CHECK: sy-subrc EQ 0.

        "Read Header Service
"*---> 01/07/2023 - Migração S4 - LO
        SORT lt_eslh by packno.
"*---> 01/07/2023 - Migração S4 - LO
        READ TABLE lt_eslh INTO lw_eslh WITH KEY packno = lw_essr-packno
                                                      BINARY SEARCH.
        CHECK: sy-subrc EQ 0.

        "Read Header Packno
"*---> 01/07/2023 - Migração S4 - LO
        SORT lt_esll_pckg by packno.
"*---> 01/07/2023 - Migração S4 - LO
        READ TABLE lt_esll_pckg INTO lw_esll_pckg WITH KEY packno = lw_essr-packno
                                                                     BINARY SEARCH.
        CHECK: sy-subrc EQ 0.

        "Read Services Item
"*---> 01/07/2023 - Migração S4 - LO
        SORT lt_esll by packno extrow.
"*---> 01/07/2023 - Migração S4 - LO
        READ TABLE lt_esll TRANSPORTING NO FIELDS WITH KEY packno = lw_esll_pckg-sub_packno
                                                           extrow = lw_po-extrow
                                                                              BINARY SEARCH.
        <fs_po_list>-lblni          = lw_essr-lblni.
        <fs_po_list>-aedat_ses      = lw_essr-aedat.      "Change Date
        <fs_po_list>-lblni          = lw_essr-lblni.      "Serv.EntrySheet
        <fs_po_list>-packno         = lw_essr-packno.     "Packno
        <fs_po_list>-introw         = lw_esll-introw.     "Internal item
        <fs_po_list>-extrow         = lw_esll-extrow.     "External item
        <fs_po_list>-srvpos         = lw_esll-srvpos.     "Service number
        <fs_po_list>-ktext1_ses     = lw_esll-ktext1.     "Serv.Text
        <fs_po_list>-menge_ses      = lw_esll-menge.      "Qty
        <fs_po_list>-meins_ses      = lw_esll-meins.      "Unit
        <fs_po_list>-tbtwr_ses      = lw_esll-tbtwr.      "Gross Value
        <fs_po_list>-netwr_ses      = lw_esll-netwr.      "Net Value
        <fs_po_list>-waers_ses      = lw_eslh-waers.      "Currency
        <fs_po_list>-act_menge_ses  = lw_esll-menge.      "PO: Entered Quantity
        <fs_po_list>-distr_ses      = lw_esll-vrtkz.      "Distribution indicator

        "Value and quantity inverted                "+CYS 08.03.2022
        IF <fs_po_list>-tbtwr_ses = 1.              "+CYS 08.03.2022
          <fs_po_list>-value_inverted = abap_true.  "+CYS 08.03.2022
        ELSE.                                       "+CYS 08.03.2022
          CLEAR <fs_po_list>-value_inverted.        "+CYS 08.03.2022
        ENDIF.                                      "+CYS 08.03.2022

      ELSEIF lw_po-vgabe EQ '1'.

        READ TABLE lt_mseg INTO lw_mseg WITH KEY mblnr = lw_po-doc_number
                                                 mjahr = lw_po-gjahr
                                                        BINARY SEARCH.
        CHECK: sy-subrc EQ 0.
        <fs_po_list>-mblnr     = lw_mseg-mblnr.
        <fs_po_list>-mjahr     = lw_mseg-mjahr.
        <fs_po_list>-zeile     = lw_mseg-zeile.
        <fs_po_list>-menge_gr  = lw_mseg-menge.
        <fs_po_list>-meins_gr  = lw_mseg-meins.
        <fs_po_list>-dmbtr_gr  = lw_mseg-dmbtr.
        <fs_po_list>-waers_gr  = lw_mseg-waers.

      ELSE.

        READ TABLE lt_esll_pckg INTO lw_esll_pckg WITH KEY packno = lw_ekpo-packno
                                                                      BINARY SEARCH.
        CHECK: sy-subrc EQ 0.
        READ TABLE lt_esll INTO lw_esll WITH KEY packno = lw_esll_pckg-sub_packno
                                                 extrow = lw_po-extrow
                                                                   BINARY SEARCH.
        CHECK: sy-subrc EQ 0.
        "Service data
        <fs_po_list>-introw         = lw_esll-introw.     "Internal item
        <fs_po_list>-extrow         = lw_esll-extrow.     "External item
        <fs_po_list>-srvpos         = lw_esll-srvpos.     "Service number
        <fs_po_list>-ktext1_ses     = lw_esll-ktext1.     "Serv.Text
        <fs_po_list>-menge_ses      = lw_esll-menge.      "Qty
        <fs_po_list>-meins_ses      = lw_esll-meins.      "Unit
        <fs_po_list>-tbtwr_ses      = lw_esll-tbtwr.      "Gross Value
        <fs_po_list>-netwr_ses      = lw_esll-netwr.      "Net Value
        <fs_po_list>-act_menge_ses  = lw_esll-menge.      "PO: Entered Quantity
        <fs_po_list>-distr_ses      = lw_esll-vrtkz.      "Distribution indicator

        "Value and quantity inverted                "+CYS 08.03.2022
        IF <fs_po_list>-tbtwr_ses = 1.              "+CYS 08.03.2022
          <fs_po_list>-value_inverted = abap_true.  "+CYS 08.03.2022
        ELSE.                                       "+CYS 08.03.2022
          CLEAR <fs_po_list>-value_inverted.        "+CYS 08.03.2022
        ENDIF.                                      "+CYS 08.03.2022

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_EXECUTION_MESSAGES.

    rt_messages = mt_mess.

  ENDMETHOD.


  METHOD GET_PREVIOUS_STEP.

    DATA:
      lt_sta TYPE TABLE OF /tcsr/t_sta.
    DATA:
      lw_sta TYPE /tcsr/t_sta.
*------------------------------------------------

    lt_sta[] = me->mt_sta[].
    DELETE lt_sta WHERE deactiv = abap_true.

    READ TABLE lt_sta INTO lw_sta WITH KEY step = iv_exec_step.
    IF sy-subrc EQ 0.

      "Current Step Status
      ew_curr_sta = lw_sta.

      "Read Previous STEP
      READ TABLE lt_sta INTO lw_sta INDEX sy-tabix - 1.
      IF sy-subrc EQ 0.
        "Previous Step Status
        ew_prev_sta = lw_sta.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GET_UNDO_STEP.

    DATA:
      lw_curr_sta TYPE /tcsr/t_sta,
      lw_prev_sta TYPE /tcsr/t_sta.
*------------------------------------------------

    me->get_previous_step(
            EXPORTING
              iv_exec_step  = iv_exec_step
            IMPORTING
              ew_curr_sta   = lw_curr_sta
              ew_prev_sta   = lw_prev_sta ).

    "CHECK: Current Step was Executed Successfully
    IF lw_curr_sta-stepstatus EQ /tcsr/c_constants=>mc_status-success
    OR lw_curr_sta-stepstatus EQ /tcsr/c_constants=>mc_status-warning.

      "UNDO Current STEP
      ev_undo_step  = iv_exec_step.
      "Set Previous Step with Previous Step
      ew_prev_sta      = lw_prev_sta.

    ELSE.

      "Check if Previous Step was executed Successfully
      "Select Step to Reversal
      IF lw_prev_sta-stepstatus EQ /tcsr/c_constants=>mc_status-success
      OR lw_prev_sta-stepstatus EQ /tcsr/c_constants=>mc_status-warning.

        "UNDO Previous Step
        ev_undo_step     = lw_prev_sta-step.
        "Set Next Step with Current Step
        ew_next_sta      = lw_curr_sta.

      ELSE.

        "UNDO Current Step
        ev_undo_step  = iv_exec_step.
        "Set Previous Step with Previous Step
        ew_prev_sta      = lw_prev_sta.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD LOAD_NFSE.

    DATA:
      lv_attr TYPE scx_attrname.
*-----------------------------------------------------------------------

    "-------------------------------------------------------------------
    " /TCSR/T_ACT - CURRENT STATUS
    "-------------------------------------------------------------------
    SELECT SINGLE *
      INTO me->mw_act
      FROM /tcsr/t_act
      WHERE guid_header = me->mv_guid_header.
    IF sy-subrc NE 0.
      lv_attr = me->mv_guid_header.
      "Set Message Key
      mo_util_xml->set_msgkey( iv_msgno    = '002'         "Error reading table &1 for GUID &2
                               in_attr1    = '/TCSR/T_ACT'
                               in_attr2    = lv_attr ).
      RAISE EXCEPTION TYPE /tcsr/cx_exception
        EXPORTING
          textid = mo_util_xml->mw_msgkey.
    ENDIF.

    "-------------------------------------------------------------------
    " /TCSR/T_HD - HEADER DATA
    "-------------------------------------------------------------------
    SELECT SINGLE *
      INTO me->mw_hd
      FROM /tcsr/t_hd
      WHERE guid_header = me->mv_guid_header.

    "-------------------------------------------------------------------    "+CYS 02.02.2022
    " /TCSR/T_NFSTX - CONDITION TAX                                         "+CYS 02.02.2022
    "-------------------------------------------------------------------    "+CYS 02.02.2022
    SELECT SINGLE *                                                         "+CYS 02.02.2022
      INTO me->mw_nfstx                                                     "+CYS 02.02.2022
      FROM /tcsr/t_nfstx                                                    "+CYS 02.02.2022
      WHERE guid_header = me->mv_guid_header.                               "+CYS 02.02.2022

    "-------------------------------------------------------------------
    " /TCSR/T_STA - STEP STATUS
    "-------------------------------------------------------------------
    SELECT *
      INTO TABLE me->mt_sta
      FROM /tcsr/t_sta
      WHERE guid_header = me->mv_guid_header
      ORDER BY guid_header stepcount.
    IF sy-subrc EQ 0.

      "-------------------------------------------------------------------
      " /TCSR/T_STEP - STEP CONFIGURATIONS
      "-------------------------------------------------------------------
      SELECT *
        INTO TABLE me->mt_step
        FROM /tcsr/t_step
        ORDER BY PRIMARY KEY.

      "Get Last Histcount
      SELECT COUNT(*)
        INTO me->mv_last_histcount
        FROM /tcsr/t_hist
       WHERE guid_header = me->mv_guid_header.

    ENDIF.

    "--------------------------------------------
    " /TCSR/T_PO - PO ASSIGNED
    "--------------------------------------------
    SELECT *
      INTO TABLE me->mt_po
      FROM /tcsr/t_po
     WHERE guid_header EQ me->mv_guid_header
     ORDER BY PRIMARY KEY.
    "If PO assign is initial, but it was found a PO in the table,
    "inserted in internal table to be considered. It happens when
    "an automatic step is executed
    IF me->mt_po_list_assig[] IS INITIAL AND me->mt_po_list_unass[] IS INITIAL.
      LOOP AT me->mt_po INTO me->mw_po.
        me->mw_po_list_assig-ebeln  = me->mw_po-ebeln.
        me->mw_po_list_assig-ebelp  = me->mw_po-ebelp.
        me->mw_po_list_assig-extrow = me->mw_po-extrow.
        IF me->mw_po-vgabe = '9'.
          me->mw_po_list_assig-lblni = me->mw_po-doc_number.
        ELSE.
          me->mw_po_list_assig-mblnr = me->mw_po-doc_number.
          me->mw_po_list_assig-mjahr = me->mw_po-gjahr.
        ENDIF.

        me->mw_po_list_assig-menge_iv = me->mw_po-menge.    "+CYS 02.02.2022
        me->mw_po_list_assig-dmbtr_iv = me->mw_po-dmbtr.    "+CYS 02.02.2022

        APPEND me->mw_po_list_assig TO me->mt_po_list_assig.
      ENDLOOP.
    ENDIF.

    "--------------------------------------------
    " /TCSR/T_RED - REFERENCE DOCUMENTS
    "--------------------------------------------
    SELECT *
      INTO TABLE me->mt_ref
      FROM /tcsr/t_ref
     WHERE guid_header EQ me->mv_guid_header
     ORDER BY PRIMARY KEY.

    "--------------------------------------------
    " /TCSR/T_RED - REFERENCE DOCUMENTS
    "--------------------------------------------
    SELECT *
      INTO TABLE me->mt_proc_doc
      FROM /tcsr/t_proc_doc
     WHERE guid_header EQ me->mv_guid_header
     ORDER BY PRIMARY KEY.

  ENDMETHOD.


  METHOD MESSAGE_FORMAT.

    "Add all error messages to bal log
    LOOP AT mt_mess ASSIGNING FIELD-SYMBOL(<fs_return>).

      MESSAGE ID <fs_return>-id
            TYPE <fs_return>-type
          NUMBER <fs_return>-number
            WITH <fs_return>-message_v1
                 <fs_return>-message_v2
                 <fs_return>-message_v3
                 <fs_return>-message_v4
            INTO <fs_return>-message.

    ENDLOOP.


  ENDMETHOD.


  METHOD PROCFLOW_EXECUTION.

    DATA:
      lw_step TYPE /tcsr/t_step.
    DATA:
      lv_method TYPE string,
      lv_job_start TYPE abap_bool.
    DATA:
      lo_param     TYPE REF TO /tcsr/c_param,
      lo_exception TYPE REF TO /tcsr/cx_exception.
*-----------------------------------------------------------------------

    CREATE OBJECT: lo_param.

*------------------------------------------------
*   GET JOBNAME + USER to execute (Job)
*------------------------------------------------
    me->mv_job_name = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                               iv_name     = 'JOB_NAME' ).
    IF me->mv_job_name IS INITIAL.
      me->mv_job_name = '/TCSR/PROCSTEP'.
    ENDIF.
    me->mv_job_user = lo_param->get_parameter( iv_programm = 'PROCESS_STEPS'
                                               iv_name     = 'JOB_USER' ).

*------------------------------------------------
*   SET - STEP to be Started
*------------------------------------------------
    "SET - STEP to be Started
    IF me->mv_exec_step IS NOT INITIAL.
      me->mv_step_start   = me->mv_exec_step.
    ELSE.
      "IF Step was not filled
      READ TABLE me->mt_sta INTO me->mw_sta INDEX 1.
      IF sy-subrc EQ 0.
        "Get First Step to be Started
        me->mv_step_start = me->mw_sta-step.
      ENDIF.
    ENDIF.


*------------------------------------------------
*   GET STEP TO START
*------------------------------------------------
    READ TABLE me->mt_sta TRANSPORTING NO FIELDS
                          WITH KEY step = me->mv_step_start.
    CHECK sy-subrc EQ 0.

*------------------------------------------------
*   EXECUTE PROCESS STEPS
*------------------------------------------------
    LOOP AT me->mt_sta INTO me->mw_sta FROM sy-tabix.

      me->mv_tabix_sta = sy-tabix.

*------------------------------------------------
*-----STEP = OK and ACTION <> U-Undo Step
*------------------------------------------------
      IF me->mw_sta-stepstatus EQ /tcsr/c_constants=>mc_status-success
     AND me->mv_action         NE /tcsr/c_constants=>mc_action-undo.
        CONTINUE. "if step already OK and not Reversal, continue
      ENDIF.

*------------------------------------------------
*-----STEP DEACTIVATED
*------------------------------------------------
      IF me->mw_sta-deactiv EQ abap_true.
        CONTINUE. "if step is deactivated continue to next step
      ENDIF.

*------------------------------------------------
*-----NEXT STEP FOR REVERSAL STEP
*------------------------------------------------
      IF me->mv_action   EQ /tcsr/c_constants=>mc_action-undo
     AND me->mw_sta-step NE me->mv_step_start.

        CLEAR: sy-msgid.
        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-initial ).
        EXIT.

      ENDIF.

*------------------------------------------------
*-----MANUAL STEP AND NOT EXECUTED YET
*------------------------------------------------
      IF me->mw_sta-autoproc   EQ /tcsr/c_constants=>c_manual_step
     AND me->mw_sta-stepstatus IS INITIAL.

        CLEAR: sy-msgid.

        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-pending ).

        me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                      iv_stepstatus = /tcsr/c_constants=>mc_status-pending ).
        EXIT.

*------------------------------------------------
*-----AUTOMATIC NEXT STEP TO BE EXECUTED (JOB)
*------------------------------------------------
      ELSEIF sy-batch            EQ ''                              "Check if execution ONLINE
         AND me->mw_sta-autoproc EQ /tcsr/c_constants=>c_auto_step  "Check step is Automatic
         AND me->mw_sta-step     NE me->mv_step_start               "Check step to be executed is next
         AND me->mv_step_start   IS NOT INITIAL                     "Check step called
         AND me->mv_job_user     IS NOT INITIAL.                    "User JOB parameterized

        CLEAR: sy-msgid.

        me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-pending ).

        me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                      iv_stepstatus = /tcsr/c_constants=>mc_status-pending ).
        lv_job_start = 'X'.

        EXIT.

*------------------------------------------------
*-----AUTOMATIC STEP in background OR FIRST STEP
*------------------------------------------------
      ELSEIF me->mw_sta-autoproc EQ /tcsr/c_constants=>c_auto_step
          OR me->mw_sta-step     EQ me->mv_step_start.

        "Select Steps Configuration
        READ TABLE me->mt_step INTO lw_step
                               WITH KEY step = me->mw_sta-step
                               BINARY SEARCH.
        IF sy-subrc NE 0 OR lw_step-stepimpl IS INITIAL.

          MESSAGE e201(/tcsr/msg) INTO me->mv_dummy "Método de Implementação da Etapa & não foi configurado
                                  WITH me->mw_sta-step.

          me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
          me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                        iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
          EXIT.
        ENDIF.

        "Set Configurated Method name
        lv_method = lw_step-stepimpl.

*------------------------------------------------
*-----  CALL METHOD - STEP
*------------------------------------------------
        TRY.
            CALL METHOD me->(lv_method).

          CATCH /tcsr/cx_exception INTO lo_exception.
            "Returned Error
            MESSAGE ID lo_exception->if_t100_message~t100key-msgid
               TYPE 'E'
             NUMBER lo_exception->if_t100_message~t100key-msgno
               WITH lo_exception->if_t100_message~t100key-attr1
                    lo_exception->if_t100_message~t100key-attr2
                    lo_exception->if_t100_message~t100key-attr3
                    lo_exception->if_t100_message~t100key-attr4
               INTO me->mv_dummy.

            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
            EXIT.

          CATCH cx_sy_dyn_call_error.
            MESSAGE e202(/tcsr/msg) INTO me->mv_dummy  "Erro ao executar Etapa &
                                    WITH me->mw_sta-step.

            me->set_stepstatus_sta_table( /tcsr/c_constants=>mc_status-error ).
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = /tcsr/c_constants=>mc_status-error ).
            EXIT.

        ENDTRY.

*------------------------------------------------
*-----  After Process Step executed
*------------------------------------------------
        CASE me->mw_sta-stepstatus.

          WHEN /tcsr/c_constants=>mc_status-success.      "01
            "Completed Step
            CONTINUE.

          WHEN /tcsr/c_constants=>mc_status-warning.      "02
            "Completed Step
            CONTINUE.

          WHEN /tcsr/c_constants=>mc_status-error.        "03
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-errordet.     "05
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-pending.      "13
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-unassigned.   "86
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-reversedoc.   "87
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-undone.       "88
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            CONTINUE.

          WHEN /tcsr/c_constants=>mc_status-stopped.      "89
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.
          WHEN /tcsr/c_constants=>mc_status-invoiced.      "100
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-canceled.     "101
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-deleted.      "102
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

          WHEN /tcsr/c_constants=>mc_status-reversed.     "103
            me->set_stepstatus_act_table( iv_step       = me->mw_sta-step
                                          iv_stepstatus = me->mw_sta-stepstatus ).
            EXIT.

        ENDCASE.

      ENDIF. "me->mw_sta-autoproc

    ENDLOOP.

    "Save Database Tables
    me->save_to_db( ).

    "DEqueue GUID_HEADER
    me->dequeue_guid( ).

*------------------------------------------------
*   JOB - Execute Steps (Automatic)
*------------------------------------------------
    IF lv_job_start IS NOT INITIAL.
      "Start JOB Step automatic
      me->execute_job_procstep( ).
    ENDIF.

  ENDMETHOD.


  METHOD PROCSTEP_CANCEL.

    CASE me->mv_action.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->send_email_cancel( ).
        me->execute_stop_process_continue( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_CLOSE.

    CASE me->mv_action.

*------------------------------------------------
*     CLOSE NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-closproc
        OR /tcsr/c_constants=>mc_action-save
        OR /tcsr/c_constants=>mc_action-continue.
        CHECK: me->execute_prereq( ) IS INITIAL.
        me->execute_close_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_DOCREF.

    CASE me->mv_action.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
*        me->execute_check_docref( ).
        me->execute_docref( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

*------------------------------------------------
*     UNDO STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-undo.
        me->execute_undo_step( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_DOWNLOAD.

    CASE me->mv_action.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_download_continue( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_FILENAME.

  ENDMETHOD.


  METHOD PROCSTEP_MIGO.

    CASE me->mv_action.

*------------------------------------------------
*     SAVE
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-save.
        me->execute_migo_save( ).

*------------------------------------------------
*     REVERSE - MBST
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-reverse.
        me->execute_migo_reverse( ).

*------------------------------------------------
*     UNDO STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-undo.
        me->execute_validate_undo( ).
        me->execute_undo_step( ).

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_migo_continue( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_MIRO.

    CASE me->mv_action.

*------------------------------------------------
*     SAVE
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-save OR
           /tcsr/c_constants=>mc_action-continue.

        CHECK: me->execute_prereq( ) IS INITIAL.

        me->execute_miro_create( ).

*------------------------------------------------
*     REVERSE - MR8M
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-reverse.
        me->execute_mr8m_reverse( ).

*------------------------------------------------
*     UNDO STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-undo.
        me->execute_undo_step( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_ML81N.

    CASE me->mv_action.

*------------------------------------------------
*     SAVE - ASSIGN/UNASSIGN/CREATE
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-save.
        me->execute_ml81n_save( ).

*------------------------------------------------
*     REVERSE - ML81N
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-reverse.
        me->execute_ml81n_delete( ).

*------------------------------------------------
*     UNDO STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-undo.
        me->execute_validate_undo( ).
        me->execute_undo_step( ).

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_ml81n_continue( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_POASSIGN.

    CASE me->mv_action.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_prereq( ).

*------------------------------------------------
*     SAVE PO ASSIGN
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-save.
        me->execute_save_poassign( ).
        me->execute_prereq( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

*------------------------------------------------
*     UNDO STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-undo.
        me->execute_undo_step( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_PORTAL.

    CASE me->mv_action.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_portal_continue( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_PREREQ.

    CASE me->mv_action.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_prereq( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_PROCDET.

    CASE me->mv_action.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_procdet1( ).

*------------------------------------------------
*     SAVE PO ASSIGN
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-save.
*        me->execute_save_process( ).
        me->execute_save_procdet( ).
        me->execute_procdet1( ).

*------------------------------------------------
*     STOP PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-stopproc.
        me->execute_stop_process( ).

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

*------------------------------------------------
*     UNDO STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-undo.
        me->execute_undo_procdet( ).

    ENDCASE.

  ENDMETHOD.


  METHOD PROCSTEP_VALIDATE.

    CASE me->mv_action.

*------------------------------------------------
*     UNDO STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-undo.
        me->execute_undo_step( ).

*------------------------------------------------
*     SAVE
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-save.
        me->execute_validate_save( ).
        CHECK: me->execute_prereq( ) IS INITIAL.

*------------------------------------------------
*     CONTINUE STEP
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-continue.
        me->execute_validate_continue( ).
        CHECK: me->execute_prereq( ) IS INITIAL.

*------------------------------------------------
*     CANCEL NFS-E PROCESS
*------------------------------------------------
      WHEN /tcsr/c_constants=>mc_action-cancproc.
        me->execute_cancel_process( ).

    ENDCASE.

  ENDMETHOD.


  METHOD SAVE_TO_DB.

    "Current Status
    IF me->mw_act_upd IS NOT INITIAL.
      MODIFY /tcsr/t_act FROM me->mw_act_upd.
    ENDIF.

    "Taxes
    IF me->mw_nfstx_upd IS NOT INITIAL.
      MODIFY /tcsr/t_nfstx FROM me->mw_nfstx_upd.
    ENDIF.

    "Step Status (delete)
    IF me->mt_sta_del[] IS NOT INITIAL.
      DELETE /tcsr/t_sta FROM TABLE me->mt_sta_del.
    ENDIF.

    "Step Status
    IF me->mt_sta_upd[] IS NOT INITIAL.
      MODIFY /tcsr/t_sta FROM TABLE me->mt_sta_upd.
    ENDIF.

    "Status History Table
    IF me->mt_hist_upd[] IS NOT INITIAL.
      MODIFY /tcsr/t_hist FROM TABLE me->mt_hist_upd.
    ENDIF.

    "Purchase Order Table (delete)
    IF me->mt_po_del[] IS NOT INITIAL.
      DELETE /tcsr/t_po FROM TABLE me->mt_po_del.
    ENDIF.

    "Purchase Order Table
    IF me->mt_po_upd[] IS NOT INITIAL.
      MODIFY /tcsr/t_po FROM TABLE me->mt_po_upd.
    ENDIF.

    "Documents to define process type(delete)
    IF me->mt_proc_doc_del[] IS NOT INITIAL.
      DELETE /tcsr/t_proc_doc FROM TABLE me->mt_proc_doc_del.
    ENDIF.

    "Documents to define process type
    IF me->mt_proc_doc_upd[] IS NOT INITIAL.
      MODIFY /tcsr/t_proc_doc FROM TABLE me->mt_proc_doc_upd.
    ENDIF.

    IF me->mw_approval IS NOT INITIAL.
      MODIFY /tcsr/t_approval FROM me->mw_approval.
    ENDIF.


    IF me->mw_act_upd        IS NOT INITIAL OR
       me->mt_sta_del[]      IS NOT INITIAL OR
       me->mt_sta_upd[]      IS NOT INITIAL OR
       me->mt_hist_upd[]     IS NOT INITIAL OR
       me->mt_po_del[]       IS NOT INITIAL OR
       me->mt_po_upd[]       IS NOT INITIAL OR
       me->mt_proc_doc_del[] IS NOT INITIAL OR
       me->mt_proc_doc_upd[] IS NOT INITIAL OR
       me->mw_approval       IS NOT INITIAL.
      COMMIT WORK AND WAIT.
    ENDIF.

    CLEAR: me->mw_act_upd,
           me->mw_approval.

    REFRESH: me->mt_sta_del[],
             me->mt_sta_upd[],
             me->mt_hist_upd[],
             me->mt_po_del[],
             me->mt_po_upd[],
             me->mt_proc_doc_upd[],
             me->mt_proc_doc_del[].

  ENDMETHOD.


  METHOD SELECT_PROCFLOW.

    DATA:
      lt_flowp     TYPE TABLE OF /tcsr/t_flowp,
      lt_flowp_aux TYPE TABLE OF /tcsr/t_flowp,
      lt_sta       TYPE TABLE OF /tcsr/t_sta,
      lt_step      TYPE TABLE OF /tcsr/t_step.
    DATA:
      lw_flowp TYPE /tcsr/t_flowp,
      lw_sta   TYPE /tcsr/t_sta.
    FIELD-SYMBOLS:
      <fs_sta> TYPE /tcsr/t_sta.
*----------------------------------------------------------------------*

*--- Use process type to determine process flow
    SELECT *
      FROM /tcsr/t_flowp
      INTO TABLE lt_flowp
      WHERE proctyp = me->mw_act-proctyp
      ORDER BY PRIMARY KEY.
    IF sy-subrc NE 0.
      MESSAGE e205(/tcsr/msg)"Nenhum fluxo de processos encontrado para Processo &
         INTO me->mv_dummy WITH me->mw_act-proctyp.
      EXIT.
    ENDIF.

*------------------------------------------------
* Set STA Table
*------------------------------------------------
    IF me->mv_proctyp_manual IS NOT INITIAL.
      SELECT *
        FROM /tcsr/t_flowp
        INTO TABLE lt_flowp_aux
        WHERE proctyp = 'DEFAULT'
        ORDER BY PRIMARY KEY.
      IF sy-subrc IS INITIAL.
        LOOP AT me->mt_sta INTO lw_sta.
          READ TABLE lt_flowp_aux TRANSPORTING NO FIELDS WITH KEY stepcount = lw_sta-stepcount.
          IF sy-subrc IS NOT INITIAL.
            DELETE me->mt_sta WHERE stepcount EQ lw_sta-stepcount.
            DELETE /tcsr/t_sta FROM lw_sta.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT lt_flowp INTO lw_flowp.

      APPEND INITIAL LINE TO lt_sta ASSIGNING <fs_sta>.
      MOVE-CORRESPONDING lw_flowp TO <fs_sta>.
      <fs_sta>-guid_header = me->mv_guid_header.
      IF <fs_sta>-deactiv = 'X'.
        <fs_sta>-stepstatus = '12'.
      ENDIF.

    ENDLOOP.

    SORT lt_sta BY stepcount.

    APPEND LINES OF lt_sta TO me->mt_sta.
    APPEND LINES OF lt_sta TO me->mt_sta_upd.

  ENDMETHOD.


  METHOD SEND_EMAIL_CANCEL.

    DATA:
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.
*------------------------------------------------

    CHECK: me->mw_sta-stepstatus IS INITIAL.

    CALL FUNCTION '/TCSR/F_SEND_EMAIL_CANCEL'
      EXPORTING
        iw_hd         = me->mw_hd
      EXCEPTIONS
        no_email_list = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      MESSAGE w455(/tcsr/msg) "Nenhum endereço de e-mail foi cadastrado
         INTO me->mv_dummy.
      me->set_stepstatus_sta_table( iv_stepstatus = /tcsr/c_constants=>mc_status-warning ).
    ENDIF.

  ENDMETHOD.


  METHOD SEND_EMAIL_STEP.

    CALL FUNCTION '/TCSR/F_SEND_EMAIL_STEP'
      EXPORTING
        iw_act           = me->mw_act
        iw_hd            = me->mw_hd
        iw_sta           = me->mw_sta
        it_po            = me->mt_po_upd[]
        it_bapireturn    = me->mt_bapireturn[]
      EXCEPTIONS
        msgid_not_active = 1
        email_not_found  = 2
        OTHERS           = 3.

  ENDMETHOD.


  METHOD SET_NEW_GUID.

    DATA: go_util_moni TYPE REF TO /tcsr/c_util_monitor,
          go_exception TYPE REF TO /tcsr/cx_exception.

* SET Guid to be process
    me->mv_guid_header      = iv_guid_header.

* SET Action to be Process
    me->mv_action           = iv_action.

*--- SET new Process Determination
    me->mv_proctyp_manual   = iv_proctyp.

* SET Guid from NFS-e Canceled
    me->mv_guid_cancel      = iv_guid_cancel.

* SET Tolerance Value
    me->mv_tolerance        = iv_tolerance.

* SET Tolerance Tax
    me->mv_tolerance_tax    = iv_tolerance_tax.

* SET Nota Fiscal Type
    me->mv_nftype           = iv_nftype.

* SET Payment Block for MIRO
    me->mv_zlspr            = iv_zlspr.

* SET Cancelation Reason
    me->mv_cancreason       = iv_cancreason.

* SET create FRS/MIGO
    me->mv_not_create           = iv_not_create.

*--- SET approval to continue process with error
    me->mv_approved_diff    = iv_approved_diff.

* SET PO_LIST table with Unassign/Assign Documents
    me->mt_po_list_unass[]  = it_po_list_unass[].
    me->mt_po_list_assig[]  = it_po_list_assig[].

* SET Text to Cancelation Reason
    me->mt_cancr_text[]     = it_cancr_text[].

    IF iv_call_step NE 'PORTAL'.
* Get XML data
      CREATE OBJECT go_util_moni
        EXPORTING
          iv_guid_header = iv_guid_header.

      TRY .
          go_util_moni->get_xml( ).
        CATCH /tcsr/cx_exception INTO go_exception.
          "Display Error message
          MESSAGE ID go_exception->if_t100_message~t100key-msgid
             TYPE 'E'
           NUMBER go_exception->if_t100_message~t100key-msgno
             WITH go_exception->if_t100_message~t100key-attr1
                  go_exception->if_t100_message~t100key-attr2
                  go_exception->if_t100_message~t100key-attr3
                  go_exception->if_t100_message~t100key-attr4.
      ENDTRY.

      me->mw_xml_rpsx = go_util_moni->mw_xml_rpsx.
    ENDIF.

* Enqueue GUID_HEADER
    me->enqueue_guid( ).

* Clear Global Attribues
    me->clear_attributes( ).

* Load NFS-e fom GUID
    me->load_nfse( ).

* INSS base
    me->mv_inss_base        = iv_inss_base.

* IR base
    me->mv_ir_base          = iv_ir_base.

* Manual Process
    me->mv_manual           = iv_manual.

* Check if was call Undo Step
    me->check_call_undo_step( iv_call_step ).

  ENDMETHOD.


  METHOD SET_PROCTYP.

    DATA: lw_proc_det TYPE /tcsr/t_proc_det,
          lw_proc_grp TYPE /tcsr/t_proc_grp,
          lw_ekko     TYPE ekko,
          lw_ekpo     TYPE ekpo,
          lw_mara     TYPE mara.
    DATA: lv_tab   TYPE tabname16,
          lv_field TYPE name_feld.
    FIELD-SYMBOLS: <fs_field> TYPE any.

    SELECT SINGLE *
      INTO lw_ekko
      FROM ekko
      WHERE ebeln = iv_ebeln.

    SELECT SINGLE *
      INTO lw_ekpo
      FROM ekpo
      WHERE ebeln = iv_ebeln AND
            ebelp = iv_ebelp.

    SELECT SINGLE *
      INTO lw_mara
      FROM mara
      WHERE matnr = lw_ekpo-matnr.

    LOOP AT me->mt_proc_det INTO lw_proc_det.
"*---> 01/07/2023 - Migração S4 - LO
      SORT me->mt_proc_grp by group_field.
"*---> 01/07/2023 - Migração S4 - LO
      READ TABLE me->mt_proc_grp INTO lw_proc_grp WITH KEY group_field = lw_proc_det-group_field
                                                                                BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT me->mt_proc_grp INTO lw_proc_grp FROM sy-tabix.
          IF lw_proc_grp-group_field NE lw_proc_det-group_field.
            EXIT.
          ENDIF.

          IF lw_proc_grp-field IS NOT INITIAL.
            CONCATENATE 'LW_' lw_proc_grp-field INTO lv_field.
            ASSIGN (lv_field) TO <fs_field>.
            IF sy-subrc = 0.
              IF lw_proc_det-value = '*' AND <fs_field> IS INITIAL.
                CONTINUE.
              ELSEIF <fs_field> NE lw_proc_det-value.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lw_proc_grp-field2 IS NOT INITIAL.
            CONCATENATE 'LW_' lw_proc_grp-field2 INTO lv_field.
            ASSIGN (lv_field) TO <fs_field>.
            IF sy-subrc = 0.
              IF lw_proc_det-value = '*' AND <fs_field> IS INITIAL.
                CONTINUE.
              ELSEIF <fs_field> NE lw_proc_det-value2.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lw_proc_grp-field3 IS NOT INITIAL.
            CONCATENATE 'LW_' lw_proc_grp-field3 INTO lv_field.
            ASSIGN (lv_field) TO <fs_field>.
            IF sy-subrc = 0.
              IF lw_proc_det-value = '*' AND <fs_field> IS INITIAL.
                CONTINUE.
              ELSEIF <fs_field> NE lw_proc_det-value3.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lw_proc_grp-field4 IS NOT INITIAL.
            CONCATENATE 'LW_' lw_proc_grp-field4 INTO lv_field.
            ASSIGN (lv_field) TO <fs_field>.
            IF sy-subrc = 0.
              IF lw_proc_det-value = '*' AND <fs_field> IS INITIAL.
                CONTINUE.
              ELSEIF <fs_field> NE lw_proc_det-value4.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lw_proc_grp-field5 IS NOT INITIAL.
            CONCATENATE 'LW_' lw_proc_grp-field5 INTO lv_field.
            ASSIGN (lv_field) TO <fs_field>.
            IF sy-subrc = 0.
              IF lw_proc_det-value = '*' AND <fs_field> IS INITIAL.
                CONTINUE.
              ELSEIF <fs_field> NE lw_proc_det-value5.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          rv_proctyp = lw_proc_det-proctyp.

        ENDLOOP.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD SET_STEPSTATUS_ACT_TABLE.


    IF me->mw_act_upd IS INITIAL.
      me->mw_act_upd = me->mw_act.
    ENDIF.

    "Only update
    IF iv_rollback IS INITIAL.
      me->mw_act_upd-last_step        = iv_step.
      me->mw_act_upd-last_stepstatus  = iv_stepstatus.
      me->mw_act_upd-action_date      = sy-datlo.
      me->mw_act_upd-action_time      = sy-timlo.
      me->mw_act_upd-action_user      = sy-uname.
    ENDIF.

  ENDMETHOD.


  METHOD SET_STEPSTATUS_STA_TABLE.

    DATA:
      lw_hist TYPE /tcsr/t_hist.
    FIELD-SYMBOLS:
      <fs_sta_upd> TYPE /tcsr/t_sta.
*------------------------------------------------

    "Update New Status
    me->mw_sta-stepstatus = iv_stepstatus.

    IF sy-msgid IS NOT INITIAL.
      me->mw_sta-msgid = sy-msgid.
      me->mw_sta-msgty = sy-msgty.
      me->mw_sta-msgno = sy-msgno.
      me->mw_sta-msgv1 = sy-msgv1.
      me->mw_sta-msgv2 = sy-msgv2.
      me->mw_sta-msgv3 = sy-msgv3.
      me->mw_sta-msgv4 = sy-msgv4.
    ELSE.
      CLEAR: me->mw_sta-msgid,
             me->mw_sta-msgty,
             me->mw_sta-msgno,
             me->mw_sta-msgv1,
             me->mw_sta-msgv2,
             me->mw_sta-msgv3,
             me->mw_sta-msgv4.
    ENDIF.

    "Set Date/time/user only for Status different of Pending or Initial
    IF iv_stepstatus EQ /tcsr/c_constants=>mc_status-initial
    OR iv_stepstatus EQ /tcsr/c_constants=>mc_status-pending.
      CLEAR: me->mw_sta-action_date,
             me->mw_sta-action_time,
             me->mw_sta-action_user.
    ELSE.

      me->mw_sta-action_date  = sy-datlo.
      me->mw_sta-action_time  = sy-timlo.
      me->mw_sta-action_user  = sy-uname.

      "Update Step History
      MOVE-CORRESPONDING me->mw_sta TO lw_hist.
      lw_hist-histcount = me->mv_last_histcount = me->mv_last_histcount + 1.
      APPEND lw_hist TO me->mt_hist_upd.

    ENDIF.

    "Check if Row of Status UPDATE was already set
    READ TABLE me->mt_sta_upd ASSIGNING <fs_sta_upd>
                              WITH KEY step = me->mw_sta-step.
    IF sy-subrc EQ 0.
      <fs_sta_upd> = me->mw_sta.
    ELSE.
      APPEND me->mw_sta TO me->mt_sta_upd.
    ENDIF.

    "Update Global table
    MODIFY me->mt_sta FROM me->mw_sta INDEX me->mv_tabix_sta.

    "--------------------------------------------
    " CHECK: ERROR MESSAGE - SEND EMAIL
    "--------------------------------------------
    me->send_email_step( ).

  ENDMETHOD.
ENDCLASS.
