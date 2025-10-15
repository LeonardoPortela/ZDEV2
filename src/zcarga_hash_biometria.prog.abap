*&---------------------------------------------------------------------*
*& Report ZCARGA_HASH_BIOMETRIA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zcarga_hash_biometria.

data image_helper type ref to zcl_image_helper.
data biometry type ref to zcl_biometry.

data picture_left_thumb type ref to cl_gui_picture.
data custom_left_thumb  type ref to cl_gui_custom_container.

data picture_user  type ref to cl_gui_picture.
data custom_user  type ref to cl_gui_custom_container.

data picture_right_thumb type ref to cl_gui_picture.
data custom_right_thumb  type ref to cl_gui_custom_container.

data: it_selected_rows type lvc_t_row,
      owner            type soud-usrnam,
      sofd_dat         type sofdd,
      wa_selected_rows type lvc_s_row.

data: folder_id       type sofdk,
      p_error         type c,
      p_path_and_file type string.


data: back like sy-ucomm value 'BAC',  " Back
      canc like sy-ucomm value 'ESC',  " Cancel
      stop like sy-ucomm value 'RET'.  " Terminate SAPoffice

data is_object  type borident.
data attachment type borident.
data documents  type standard table of sood4.

data document   type sood4.
data ep_attachment type char40.

data l_cancelled   like sonv-flag.
data bin_filesize  like soxwd-doc_length.
data file_format   like rlgrap-filetype.
data path_and_file type string.
data object_type   like soodk-objtp.
data put_to_kpro   like sonv-flag.
data p_objcont     type table of soli.
data w_objcont     type soli.
data p_objhead     type table of soli.
data p_objpara     type table of selc.
data p_objparb     type table of soop1.

data doc_id      like soodk.
data att_id      like soodk.
data hd_dat      like sood1.
data fm_dat      like sofm1.
data new_doc_id  like soodk.
data new_att_id  like soodk.
data new_hd_dat  like sood2.
data new_fm_dat  like sofm2.
data old_doc_id  like soodk.
data fol_dat     like sofdd.
data old_enccnt  like sood-enccnt.
data attach_list like sood5 occurs 0 with header line.
data link_list   like soodk occurs 0 with header line.
data l_reappear  like sonv-flag.
data l_answer.
data l_filename           type string.
data reference_type_kpro value 'K'.      "KPro reference
data obj_type             like soodk-objtp.
data owner_dat            like soud3.

data  p_header_data like sood2.
data  p_folmem_data like sofm2.
data  l_name type string.                                   "1041757 >>
data  g_document     like sood4.
data  lo_objhead type ref to cl_bcs_objhead.

data: wa_zmmt0087 type zmmt0087,
      vlen        type i,
      vpos        type i,
      vcal        type i.

data var_string type string.
data var_255    type so_text255.



parameters p_grava type c.
parameters p_rsnum type zmmt0087-rsnum.

if p_rsnum is not initial.
  select single *
    from zmmt0087
    into wa_zmmt0087
    where rsnum = p_rsnum.

  if sy-subrc ne 0.
    message 'Reserva não encontrada' type 'S'.
    exit.
  endif.
endif.

if p_grava is not initial and p_grava ne 'D'.

  call function 'SO_FOLDER_ROOT_ID_GET'
    exporting
      region    = 'B'
    importing
      folder_id = folder_id
    exceptions
      others    = 0.

  if owner is initial.
    clear owner_dat.
    move sy-uname to owner_dat-sapnam.
    call function 'SO_NAME_CONVERT'
      exporting
        name_in               = owner_dat
        no_address_name       = 'X'
      importing
        name_out              = owner_dat
      exceptions
        communication_failure = 71
        office_name_not_exist = 19
        parameter_error       = 23
        sap_name_not_exist    = 29
        system_failure        = 72
        user_not_exist        = 34.
    if sy-subrc ne 0.
      p_error = 'X'.
      return.
    endif.

    move owner_dat-usrnam to owner.

    call function 'SO_FOLDER_HEADER_READ'
      exporting
        folder_id                  = folder_id
        owner                      = owner
      importing
        folder_data                = sofd_dat
      exceptions
        communication_failure      = 71
        folder_not_exist           = 6
        operation_no_authorization = 21
        system_failure             = 72.

    if sy-subrc ne 0.
      p_error = 'X'.
      return.
    endif.

  endif.

  g_document-foltp = folder_id-foltp.
  g_document-folyr = folder_id-folyr.
  g_document-folno = folder_id-folno.
  g_document-folrg = sofd_dat-folrg.

  "RAWSTRING
  data: lv_length type i,
        lt_binary type standard table of x255.

  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer        = wa_zmmt0087-im_polegar_log
    importing
      output_length = lv_length
    tables
      binary_tab    = lt_binary.


  clear var_string.
  call function 'SCMS_BINARY_TO_STRING'
    exporting
      input_length = lv_length
    importing
      text_buffer  = var_string
    tables
      binary_tab   = lt_binary
    exceptions
      failed       = 1
      others       = 2.

  clear lv_length.
  w_objcont-line = 'IM_POLEGAR_LOG'.
  append w_objcont to p_objcont.
  loop at lt_binary into data(wa_binary).
    clear w_objcont-line.
    var_string = |{ wa_binary }|.
    CONDENSE var_string NO-GAPS.
    w_objcont-line = var_string+0(255).
    append w_objcont to p_objcont.
    clear w_objcont-line.

    w_objcont-line = var_string+255(255).
    append w_objcont to p_objcont.
    add 510 to lv_length.
  endloop.

  w_objcont-line = 'HASH_VALIDATION'.
  append w_objcont to p_objcont.
  vlen = strlen( wa_zmmt0087-hash_validation ).
  lv_length = lv_length + vlen.
  vpos = 0.
  while vpos le vlen.
    vcal = vlen - vpos.
    if vcal lt 255.
      w_objcont = wa_zmmt0087-hash_validation+vpos(vcal).
      exit.
    else.
      w_objcont = wa_zmmt0087-hash_validation+vpos(255).
    endif.
    add 255 to vpos.
    append w_objcont to p_objcont.
  endwhile.

  w_objcont-line = 'POLEGAR_LOG'.
  append w_objcont to p_objcont.
  vlen = strlen( wa_zmmt0087-polegar_log ).
  lv_length = lv_length + vlen.
  vpos = 0.
  while vpos le vlen.
    vcal = vlen - vpos.
    if vcal lt 255.
      w_objcont = wa_zmmt0087-polegar_log+vpos(vcal).
      exit.
    else.
      w_objcont = wa_zmmt0087-polegar_log+vpos(255).
    endif.
    add 255 to vpos.
    append w_objcont to p_objcont.
  endwhile.

  add 41 to lv_length.


  hd_dat-file_ext = object_type.
  hd_dat-objnam   = 'BIOMETRIA'.
  obj_type = 'EXT'.
  hd_dat-objlen = lv_length.


  call function 'SO_OBJECT_INSERT'
    exporting
      folder_id                  = folder_id
      object_type                = obj_type
      object_hd_change           = hd_dat
      object_fl_change           = fm_dat
      owner                      = owner
    importing
      object_id                  = new_doc_id
      object_hd_display          = new_hd_dat
      object_fl_display          = new_fm_dat
    tables
      objcont                    = p_objcont
      objhead                    = p_objhead
      objpara                    = p_objpara
      objparb                    = p_objparb
    exceptions
      active_user_not_exist      = 35
      communication_failure      = 71
      component_not_available    = 1
      dl_name_exist              = 3
      folder_no_authorization    = 5
      folder_not_exist           = 6
      object_type_not_exist      = 17
      operation_no_authorization = 21
      owner_not_exist            = 22
      parameter_error            = 23
      substitute_not_active      = 31
      substitute_not_defined     = 32
      system_failure             = 72
      x_error                    = 1000.

  if sy-subrc > 0.
    p_error = 'X'.
    return.
  else.
    move: new_doc_id-objtp to g_document-objtp,
          new_doc_id-objyr to g_document-objyr,
          new_doc_id-objno to g_document-objno,
          new_hd_dat       to p_header_data,
          new_fm_dat       to p_folmem_data.
    g_document-objnam = new_hd_dat-objnam.
    g_document-objdes = new_hd_dat-objdes.
    g_document-okcode = 'CREA'.
    if sy-batch is initial and sy-binpt is initial.
      message s109(so).
    endif.
  endif.

  is_object-objtype = 'BIOMETRIA'.
  is_object-objkey  = | { wa_zmmt0087-rsnum } { wa_zmmt0087-rspos }  |.

  if g_document-okcode = 'CREA' or g_document-okcode = 'CHNG'.
    attachment-objtype = 'MESSAGE'.
    attachment-objkey  =  g_document(34).
    call function 'BINARY_RELATION_CREATE_COMMIT'
      exporting
        obj_rolea    = is_object
        obj_roleb    = attachment
        relationtype = 'ATTA'
      exceptions
        others       = 1.
    if sy-subrc = 0.
      message 'Arquivo(s) Anexado(s)' type 'S'.
    endif.
  endif.
else.
  data : wa_object       type sibflporb,
         wa_object_b     type sibflporb,
         int_rel_options type obl_t_relt,
         wa_rel_options  type obl_s_relt,
         int_links       type obl_t_link.


  data: gs_folderid type          soodk,
        gs_objectid type          soodk,
        gs_note_hd  type          sood2,
        gt_note_ln  type table of soli with header line.

  wa_rel_options-low = 'ATTA'. "" Attachemnts
  wa_rel_options-sign = 'I'.
  wa_rel_options-option = 'EQ'.
  append wa_rel_options to int_rel_options.

  wa_object = value #( instid = | { wa_zmmt0087-rsnum } { wa_zmmt0087-rspos } |
                       typeid = 'BIOMETRIA'
                       catid  = 'BO' ).

  call method cl_binary_relation=>read_links_of_binrels
    exporting
      is_object           = wa_object          " Start object
      it_relation_options = int_rel_options    " Link Types
      ip_role             = 'GOSAPPLOBJ'       " Role type
    importing
      et_links            = int_links.

  data : lv_doc_id      type sofolenti1-doc_id,
         int_cont_bin   type table of solisti1,
         wat_cont_bin   type solisti1,
         int_cont_solix type table of solix,
         wa_doc_data    type sofolenti1.

  data: lv_lines    type char20,
        lv_doc_size type i,
        lv_xstring  type xstring.

  loop at int_links into data(wa_links).
    lv_doc_id = wa_links-instid_b.

    refresh int_cont_bin[].
    refresh int_cont_solix[].

    call function 'SO_DOCUMENT_READ_API1'
      exporting
        document_id                = lv_doc_id
      importing
        document_data              = wa_doc_data
      tables
        contents_hex               = int_cont_solix
        object_content             = int_cont_bin
      exceptions
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        others                     = 4.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
    if p_grava = 'D'.
      call function 'SO_DOCUMENT_DELETE_API1'
        exporting
          document_id                = wa_doc_data-doc_id
        exceptions
          document_not_exist         = 1
          operation_no_authorization = 2
          parameter_error            = 3
          x_error                    = 4
          enqueue_error              = 5
          others                     = 6.

      wa_object_b = value #( instid = wa_doc_data-doc_id
                             typeid = 'MESSAGE'
                             catid  = 'BO' ).
      try.
          cl_binary_relation=>delete_link( is_object_a = wa_object
                                           is_object_b = wa_object_b

                                           ip_reltype  = 'ATTA' ).
          commit work and wait.
        catch cx_obl_parameter_error cx_obl_model_error cx_obl_internal_error.
      endtry.
    else.
      clear: wa_zmmt0087-hash_validation, wa_zmmt0087-polegar_log,wa_zmmt0087-im_polegar_log.
      data vtipo(20).

      clear var_string.
      loop at int_cont_bin into wat_cont_bin.
        if wat_cont_bin-line = 'HASH_VALIDATION'.
          vtipo = 'HASH_VALIDATION'.
          continue.
        elseif wat_cont_bin-line = 'POLEGAR_LOG'.
          vtipo = 'POLEGAR_LOG'.
          continue.
        elseif wat_cont_bin-line = 'IM_POLEGAR_LOG'.
          vtipo = 'IM_POLEGAR_LOG'.
          continue.
        endif.
        if vtipo = 'HASH_VALIDATION'.
          wa_zmmt0087-hash_validation = wa_zmmt0087-hash_validation && wat_cont_bin-line.
        elseif vtipo = 'POLEGAR_LOG'.
          wa_zmmt0087-polegar_log = wa_zmmt0087-polegar_log && wat_cont_bin-line.
        elseif vtipo = 'IM_POLEGAR_LOG'.
          var_string = var_string && wat_cont_bin-line.
        endif.
      endloop.
      wa_zmmt0087-im_polegar_log = var_string.
      call function 'SCMS_STRING_TO_XSTRING'
        exporting
          text   = var_string
        importing
          buffer = wa_zmmt0087-im_polegar_log
        exceptions
          failed = 1
          others = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.


    endif.


  endloop.
  if p_grava ne 'D'.
    create object biometry.
    create object image_helper.
    data(_url) =
          biometry->get_digital_as_url_image2(
          image_xstring = wa_zmmt0087-im_polegar_log ).

    if wa_zmmt0087-lado_log  = 'R'.
      image_helper->display(
        exporting
          custom_name      = 'CUSTOM_RIGHT_THUMB'
          url              = _url
        changing
          custom_instance  = custom_right_thumb
          picture_instance = picture_right_thumb
      ).
    else.
      image_helper->display(
       exporting
         custom_name      = 'CUSTOM_RIGHT_THUMB'
         url              = _url
       changing
         custom_instance  = custom_left_thumb
         picture_instance = picture_left_thumb
     ).
    endif.
    call screen 0001.
  endif.


endif.
*&---------------------------------------------------------------------*
*&      Module  INPUT_MAIN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module input_main input.
  case sy-ucomm.
    when 'BACK' or 'CANCEL'.
      leave to screen 0.
  endcase.
endmodule.
