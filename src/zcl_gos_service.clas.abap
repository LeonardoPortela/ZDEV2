class ZCL_GOS_SERVICE definition
  public
  inheriting from CL_GOS_SERVICE
  final
  create public .

public section.

  methods EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GOS_SERVICE IMPLEMENTATION.


  METHOD execute.

    ##NEEDED
    DATA: lt_file_table  TYPE filetable,
          lv_rc          TYPE i,
          lv_action      TYPE i,
          lv_len         TYPE so_doc_len,
          lt_content     TYPE soli_tab,
          ls_fol_id      TYPE soodk,
          ls_obj_id      TYPE soodk,
          ls_obj_data    TYPE sood1,
          lt_objhead     TYPE STANDARD TABLE OF soli,
          lv_ext         TYPE string,
          lv_fname       TYPE string,
          lv_path        TYPE string,
          lv_path1       TYPE string,
          lv_filename    TYPE string,
          ls_note        TYPE borident,
          lv_ep_note     TYPE borident-objkey,
          ls_object      TYPE borident,
          lo_objhead     TYPE REF TO cl_bcs_objhead,
          lv_object_type TYPE soodk-objtp,
          lv_put_to_kpro TYPE sonv-flag,
          lv_filetype    TYPE rlgrap-filetype.

*   import multiple files from PC
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = 'Import Files' ##NO_TEXT
        initial_directory       = 'c:\'
        multiselection          = 'X'
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    ##NEEDED
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

*   Logic to add to multiple attachments
    LOOP AT lt_file_table ASSIGNING FIELD-SYMBOL(<lfs_file>).
      CLEAR: lv_path, lv_filename, lv_fname, lv_ext, lv_len, lt_content, ls_fol_id,
             ls_obj_data, ls_obj_id, ls_note, lo_objhead, lv_object_type,lv_put_to_kpro,
             lv_filetype, lt_objhead .
      lv_path = <lfs_file>-filename.
      TRY .
*         method to split directory and filename
          cl_bcs_utilities=>split_path( EXPORTING iv_path = lv_path IMPORTING ev_path = lv_path1 ev_name = lv_filename ).
*         method to split filename to name & extension
          cl_bcs_utilities=>split_name( EXPORTING iv_name = lv_filename IMPORTING ev_name = lv_fname ev_extension = lv_ext ).
        CATCH cx_bcs.
          CLEAR: lv_path,lv_path1,lv_filename, lv_fname, lv_ext.
      ENDTRY.

      IF lv_path IS NOT INITIAL.
        CALL FUNCTION 'SO_OBJECT_UPLOAD'
          EXPORTING
            filetype                = lv_filetype
            path_and_file           = lv_path
            no_dialog               = 'X'
          IMPORTING
            filelength              = lv_len
            act_filetype            = lv_filetype
            act_objtype             = lv_object_type
            file_put_to_kpro        = lv_put_to_kpro
          TABLES
            objcont                 = lt_content
          EXCEPTIONS
            file_read_error         = 1
            invalid_type            = 2
            x_error                 = 3
            object_type_not_allowed = 4
            kpro_insert_error       = 5
            file_to_large           = 6
            OTHERS                  = 7.
        ##NEEDED
        IF sy-subrc <> 0.
*         Implement suitable error handling here
        ENDIF.

        lo_objhead = cl_bcs_objhead=>create( lt_objhead[] ).
        lo_objhead->set_filename( lv_filename ).
        lo_objhead->set_format( lv_filetype ).
        lo_objhead->set_vsi_profile( cl_bcs_vsi_profile=>get_profile( ) ).
        lt_objhead[] = lo_objhead->mt_objhead.

*       get the folder id where to add attachment for the BO
        ##FM_SUBRC_OK
        CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
          EXPORTING
            region    = 'B'
          IMPORTING
            folder_id = ls_fol_id
          EXCEPTIONS
            OTHERS    = 1.
        ##NEEDED
        IF sy-subrc <> 0.

        ENDIF.


        ls_obj_data-objdes   = lv_fname.
        ls_obj_data-file_ext = lv_object_type.
        ls_obj_data-objlen   = lv_len.
        IF NOT lv_put_to_kpro IS INITIAL.
          ls_obj_data-extct = 'K'.
        ENDIF.

*       add the attachment data to the folder
        ##FM_SUBRC_OK
        CALL FUNCTION 'SO_OBJECT_INSERT'
          EXPORTING
            folder_id             = ls_fol_id
            object_type           = 'EXT'
            object_hd_change      = ls_obj_data
          IMPORTING
            object_id             = ls_obj_id
          TABLES
            objhead               = lt_objhead
            objcont               = lt_content
          EXCEPTIONS
            active_user_not_exist = 35
            folder_not_exist      = 6
            object_type_not_exist = 17
            owner_not_exist       = 22
            parameter_error       = 23
            OTHERS                = 1000.
        ##NEEDED
        IF sy-subrc <> 0.

        ENDIF.

        ls_object-objkey  = gs_lporb-instid.
        ls_object-objtype = gs_lporb-typeid.
        ls_note-objtype   = 'MESSAGE'.

        CONCATENATE ls_fol_id-objtp ls_fol_id-objyr ls_fol_id-objno ls_obj_id-objtp ls_obj_id-objyr ls_obj_id-objno INTO ls_note-objkey.

*       link the folder data and BO for attachment in gos
        ##FM_SUBRC_OK
        CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
          EXPORTING
            obj_rolea    = ls_object
            obj_roleb    = ls_note
            relationtype = 'ATTA'
          EXCEPTIONS
            OTHERS       = 1.
        ##NEEDED
        IF sy-subrc <> 0.

        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lt_file_table IS NOT INITIAL .
      MESSAGE s043(sgos_msg).
      go_model->set_check_atta_list( ).
    ELSE.
      MESSAGE s042(sgos_msg).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
