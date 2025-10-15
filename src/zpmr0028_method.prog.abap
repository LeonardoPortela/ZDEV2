*&---------------------------------------------------------------------*
*&  Include           ZPMR0028_METHOD
*&---------------------------------------------------------------------*

initialization.
  data(obj_main)   = new zcl_main( ).
  data(obj_even)   = new zcl_events( ).

class zcl_events implementation.

  method on_dt_ch.
  endmethod.

  method on_onf4.

    case e_fieldname.
      when 'PERNR' or 'GRUND'.

        wl_help_info =
        value #(
                 call      = cond #( when e_fieldname eq 'PERNR' then 'M'     else 'T' )
                 object    = 'F'
                 program   = sy-repid
                 dynpro    = '100'
                 tabname   = cond #( when e_fieldname eq 'PERNR' then 'RP50G' else 'AFRUD' )
                 fieldname = e_fieldname
                 fieldtype = cond #( when e_fieldname eq 'PERNR' then 'NUMC'  else 'CHAR' )
                 mcobj     = cond #( when e_fieldname eq 'PERNR' then 'PREM'  else '' )
                 spras     = sy-langu
                 menufunct = 'HC'
                 dynpprog  = sy-repid
                 selectart = cond #( when e_fieldname eq 'PERNR' then 'A'     else 'F' )
               ).

        if e_fieldname ne 'PERNR'.
          tl_dynpselect = value #( ( fldname = 'WERKS' fldinh = obj_main->header-planplant dyfldname = 'AFRUD-WERKS' ) ).
        endif.

        obj_main->help_start( input = es_row_no-row_id field = e_fieldname ).
        obj_main->refresh( ).

      when 'ACTIVITY'.
        obj_main->f4_activity( field = e_fieldname index = es_row_no-row_id ).
      when others.
        obj_main->code_f4( input = es_row_no-row_id field = e_fieldname ).
    endcase.

    obj_main->add_line( ).
    obj_main->refresh( ).

  endmethod.

  method on_dt_ch_fs.

    check et_good_cells is not initial.

    case et_good_cells[ 1 ]-fieldname.
      when 'PERNR'.
        obj_main->get_pernr( et_good_cells[ 1 ]-row_id ).
      when 'GRUND'.
        obj_main->get_grund( et_good_cells[ 1 ]-row_id ).
      when 'FIN_CONF'.
        obj_main->set_block( ).
      when 'ACTIVITY'.
        obj_main->get_activity(
          input  = et_good_cells[ 1 ]-value
          input1 = et_good_cells[ 1 ]-row_id
        ).
      when others.

        obj_main->get_desc(
          input  = et_good_cells[ 1 ]-row_id
          input1 = et_good_cells[ 1 ]-value
          field  = et_good_cells[ 1 ]-fieldname
        ).
    endcase.

    obj_main->add_line( ).

    obj_main->refresh( ).

  endmethod.

  method on_click.
  endmethod.

endclass.

class zcl_main implementation.

  method screen.
    call screen '0100'.
  endmethod.

  method get_header.
    data: cont_oper type p decimals 2.
    clear cont_oper.
    clear wa_aufk.

    if v_ordem is initial.
      clear: v_vornr, v_nota.
      free: it_niobj, it_nidef, it_nicau, it_nitas, it_nifac, header.
      obj_main->alv2->refresh_table_display( exporting is_stable = obj_main->stable ).
      obj_main->alv3->refresh_table_display( exporting is_stable = obj_main->stable ).
      obj_main->alv4->refresh_table_display( exporting is_stable = obj_main->stable ).
      obj_main->alv5->refresh_table_display( exporting is_stable = obj_main->stable ).
      obj_main->alv6->refresh_table_display( exporting is_stable = obj_main->stable ).
      message i010 display like 'E'.
    else.

      call function 'BAPI_ALM_ORDER_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
        exporting                               "#EC CI_USAGE_OK[2669857]
          number        = input
        importing
          es_header     = header
        tables
          et_operations = it_operat
*         ET_OPERATIONS = IT_APONTA
          et_olist      = it_olist
          return        = it_return.

      if header is initial.
        message s000(zppm001) display like 'E' with 'Ordem não encontrada'.
        return = 4.
        leave to current transaction.
      else.

        select a~vaplz a~werks a~aufnr a~ktext b~aufpl c~objnr c~vornr c~ltxa1 " D~AUERU
        from aufk as a
        inner join afko as b on b~aufnr = a~aufnr
        inner join afvc as c on c~aufpl = b~aufpl
        into corresponding fields of table t_aufk
        where a~aufnr eq header-orderid
        and a~autyp eq '30'
        and a~phas1 eq abap_true.

        if t_aufk is initial.
          message i000(o0) with header-orderid 'não esta liberada'.
        else.
          free t_aufk.
          select a~vaplz a~werks a~aufnr a~ktext b~aufpl c~objnr c~vornr c~ltxa1 " D~AUERU
          from aufk as a
          inner join afko as b on b~aufnr = a~aufnr
          inner join afvc as c on c~aufpl = b~aufpl
          into corresponding fields of table t_aufk
          where a~aufnr eq header-orderid
          and a~autyp eq '30'
          and c~steus in ( 'PM01', 'PM02' )
          and a~phas1 eq abap_true.

          if t_aufk is initial.
            message e097(ru) with header-orderid.
          else.

            select *
            from afru
            into corresponding fields of table t_afru
            for all entries in t_aufk
              where aufnr eq t_aufk-aufnr
              and vornr eq t_aufk-vornr
              and aueru eq abap_true.
            sort  t_afru ascending by aufnr vornr.

            loop at t_afru assigning field-symbol(<w_afru>).
              loop at t_aufk assigning field-symbol(<w_aufk>) where aufnr = <w_afru>-aufnr
                                                                and vornr = <w_afru>-vornr.
                if sy-subrc = 0.
*            IF LINE_EXISTS( T_AUFK[ AUFNR = <W_AFRU>-AUFNR ] ).
                  delete  t_aufk index sy-tabix. continue.
                endif.
              endloop.
            endloop.

            if  t_aufk is initial.
              message e049(ru) with header-orderid 'OPERAÇÃO' v_vornr.
            else.
              if v_vornr is not initial.
                read table t_aufk into wa_aufk with key aufnr = header-orderid
                                                        vornr = v_vornr.

                if sy-subrc <> 0.
                  message e283(ru) with v_vornr header-orderid.
                endif.
              else.
                loop at t_aufk assigning <w_aufk>.
                  if <w_aufk>-vornr is not initial.
                    add 1 to cont_oper.
                  endif.
                endloop.

                if cont_oper > 1.
                  call screen 0500 starting at 5 5 ending at 86 24.
                else.
                  read table t_aufk into wa_aufk with key aufnr = header-orderid.
                  v_vornr = wa_aufk-vornr.
                  clear wa_aufk.
                endif.

                read table t_aufk into wa_aufk with key aufnr = header-orderid
                                                        vornr = v_vornr.
                if sy-subrc = 0.
                  v_ktext = wa_aufk-ltxa1.

                  data: catalog_profile type bapi10011e,
                        return1         type bapireturn,
                        codes           type table of  bapi10011t.


                  if  header-notif_no is not initial.

                    header-notif_no = |{ header-notif_no alpha = in }|.
                    v_nota          = header-notif_no.
                    v_nota_descr    = header-short_text.

                    call function 'BAPI_SERVNOT_GETCATALOGPROFIL'
                      exporting
                        number          = header-notif_no
                        language        = sy-langu
                      importing
                        catalog_profile = catalog_profile
                        return          = return1
                      tables
                        codes           = codes.

                    codes1 = value #( for ls in codes where ( cat_typ eq 'B' or
                                                              cat_typ eq 'C' or
                                                              cat_typ eq '5' or
                                                              cat_typ eq '2' or
                                                              cat_typ eq 'A' )
                                      ( ls )
                                    ).

                    me->set_operations( ).

                    check me->get_item( header-notif_no ) is initial.

                    me->set_item( ).

*                  me->add_line( ).
                  else.
                    clear: v_nota, v_nota_descr.
                    free : it_niobj,it_nidef, it_nicau, it_nitas, it_nifac.
                  endif.

                  if v_vornr is not initial.
                    perform zapontamento.
                  endif.

                endif.
              endif.
            endif.
          endif.
        endif.
      endif.
    endif.
  endmethod.

  method get_item.

    free: it_notiteme, it_notcause, it_notactve, it_nottaske.

    call function 'BAPI_ALM_NOTIF_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
      exporting
        number             = input
      importing
        notifheader_export = it_notif
        notifhdtext        = it_notif_h
      tables
        notitem            = it_notiteme
        notifcaus          = it_notcause
        notifactv          = it_notactve
        notiftask          = it_nottaske.

    if it_notif is initial.
*      MESSAGE i000(o0) WITH 'Nota não encontrada'.
      return = 4.
    else.
      free ld_number.

      ld_number = value #( notif_no = it_notif-notif_no
                          equipment = it_notif-equipment
                         notif_type = it_notif-notif_type
                         short_text = it_notif-short_text
                           priotype = it_notif-priotype
                         sys_status = it_notif-sys_status ).



      ld_syststat = value #( langu = 'PT'
                          languiso = ''
                           refdate = sy-datum
                           reftime = sy-uzeit ).

    endif.
  endmethod.

  method set_operations.

    free: it_opera.

    it_opera = value #( for ls in  it_operat
                          (
                            activity             = ls-activity
                            sub_activity         = ls-sub_activity
                            description          = ls-description
                            work_cntr            = ls-work_cntr
                            duration_normal_unit = ls-duration_normal_unit
                            acttype              = ls-acttype
                          )
                      ).

  endmethod.

  method set_item.

    it_niobj = value #( for l1 in it_notiteme where ( delete_flag is initial ) ( corresponding #( l1 ) ) ).
*    LOOP AT it_niobj ASSIGNING FIELD-SYMBOL(<l1>). <l1>-check = abap_true. ENDLOOP.

    it_nidef = value #( for l2 in it_notiteme where ( delete_flag is initial ) ( corresponding #( l2 ) ) ).
*    LOOP AT it_nidef ASSIGNING FIELD-SYMBOL(<l2>). <l2>-check = abap_true. ENDLOOP.

    it_nicau = value #( for l3 in it_notcause where ( delete_flag is initial ) ( corresponding #( l3 ) ) ).
*    LOOP AT it_nicau ASSIGNING FIELD-SYMBOL(<l3>). <l3>-check = abap_true. ENDLOOP.

    it_nitas = value #( for l4 in it_nottaske where ( delete_flag is initial ) ( corresponding #( l4 ) ) ).
*    LOOP AT it_nitas ASSIGNING FIELD-SYMBOL(<l4>). <l4>-check = abap_true. ENDLOOP.

    it_nifac = value #( for l5 in it_notactve where ( delete_flag is initial ) ( corresponding #( l5 ) ) ).
*    LOOP AT it_nifac ASSIGNING FIELD-SYMBOL(<l5>). <l5>-check = abap_true. ENDLOOP.

  endmethod.

  method set_estrutura.

    check me->get_header( input ) is initial.

  endmethod.

  method pbo.

    it_ucomm =  cond #(
      when sy-ucomm eq 'EDIT'
                          then value #(
                                        ( ucomm = 'SAVE'    )
                                        ( ucomm = 'ENCORD'  )
                                        ( ucomm = 'ENCONO'  )
                                        ( ucomm = 'DEL'     )                        ) else value #(
                                        ( ucomm = 'APONTAR' )
                                        ( ucomm = 'EDIT'    ) ) ).


    if obj_main->header-orderid is initial.
      data: w_ucomm type sy-ucomm.

      w_ucomm = 'APONTAR'.
      append w_ucomm to it_ucomm.
      clear w_ucomm.

      w_ucomm = 'DEL'.
      append w_ucomm to it_ucomm.
      clear w_ucomm.
    endif.

    if obj_main->header-notif_no is initial.
      w_ucomm = 'EDIT'.
      append w_ucomm to it_ucomm.
      clear w_ucomm.
    endif.

    if it_aponta is initial.
      w_ucomm = 'DEL'.
      append w_ucomm to it_ucomm.
      clear w_ucomm.
    endif.

    set pf-status 'PF0100'  excluding it_ucomm.
    set titlebar 'TI0100'.

    me->set_block( ).
    me->set_alv1( ).

    if v_ordem is not initial.
      if it_timetickets is initial.
        if sy-ucomm = 'ENTER' or sy-ucomm = 'V_CONFIRMA'.
          perform zapontamento.
        endif.
      endif.
    endif.
  endmethod.

  method set_layout.

    free: layout, variant, stable, it_exc.

    layout = value #(
                     zebra      = abap_false
                     no_rowins  = abap_true
                     stylefname = 'ESTILO'
                    ).

    variant = value #(
                      report = sy-repid
                     ).

    stable = value #(
                     row = abap_true
                     col = abap_true
                    ).

    it_exc = value #(
                     ( cl_gui_alv_grid=>mc_fc_excl_all )
                    ).

  endmethod.

  method set_fcat.

    free fcat.

    assign input to field-symbol(<fs_str>).
    create data str type (<fs_str>).

    fcat = corresponding lvc_t_fcat( cl_salv_data_descr=>read_structdescr( cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

    delete fcat where fieldname eq 'ESTILO'.

    loop at fcat assigning field-symbol(<fcat>).
*      <FCAT>-EDIT = ABAP_TRUE.

      case <fcat>-fieldname.
        when 'DESCRIPTION' or 'KTEXT' or 'SNAME' or 'GRDTX'.
          <fcat>-outputlen = '20'.
        when 'PERNR' or 'GRUND' or 'ACTIVITY'.
          <fcat>-f4availabl = abap_true.
*          <FCAT>-EDIT = ABAP_TRUE.
          <fcat>-outputlen = <fcat>-outputlen + 1.
        when 'ISDD' or 'ISDZ' or 'IEDD' or 'IEDZ'.
          <fcat>-f4availabl = abap_true.
          <fcat>-ref_table = <fcat>-tabname.
          <fcat>-ref_field = <fcat>-fieldname.
*          <FCAT>-EDIT = ABAP_TRUE.
        when 'SUB_ACTIVITY'.
*          <FCAT>-EDIT = ABAP_FALSE.
        when 'TXT_OBJPTCD' or 'TXT_PROBCD' or 'TXT_CAUSECD' or 'TXT_TASKCD' or 'TXT_ACTCD'.
*          <FCAT>-EDIT = ABAP_FALSE.
        when 'ACTTYPE' or 'CHECK'.
          <fcat>-no_out = abap_true.
        when 'FIN_CONF' or 'CLEAR_RES'.
          <fcat>-checkbox = abap_true.
          <fcat>-just = 'C'.
*          <FCAT>-EDIT = ABAP_TRUE.
          <fcat>-outputlen = '4'.
        when others.
          <fcat>-outputlen = <fcat>-outputlen + 1.
*          <FCAT>-EDIT = ABAP_TRUE.
      endcase.

      if <fcat>-fieldname cs 'CAT_TYP' or <fcat>-fieldname cs 'SORT_NO' or <fcat>-fieldname cs '_KEY'.
        if <fcat>-fieldname  ne 'ITEM_KEY'.
*          <FCAT>-NO_OUT = ABAP_TRUE.
        endif.
      endif.

      if <fcat>-fieldname cs 'TEXT' or <fcat>-fieldname cs 'TXT_'  or <fcat>-fieldname eq 'DESCRIPT'.
        if <fcat>-fieldname eq 'TXT_PROBCD'.
          <fcat>-outputlen = '56'.
*          <FCAT>-EDIT = ABAP_FALSE.
        else.
          <fcat>-outputlen = '28'.
        endif.
      endif.

      if <fcat>-fieldname cs 'CODE'.
        <fcat>-f4availabl = abap_true.
*        <FCAT>-EDIT = ABAP_TRUE.
      endif.

    endloop.

  endmethod.

  method set_block.

    if me->acao eq 'EDIT'.
      status = ativo.
    else.
      status = inativo.
    endif.

    loop at it_opera assigning field-symbol(<opera>).
      free: estilo.

      estilo = value #(
                         ( fieldname = 'SUB_ACTIVITY'                style = inativo )
                         ( fieldname = 'DESCRIPTION'                 style = inativo )
                         ( fieldname = 'SNAME'                       style = inativo )
                         ( fieldname = 'GRDTX'                       style = inativo )

                         ( fieldname = 'ACTIVITY'                    style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'WORK_CNTR'                   style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'PERNR'                       style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'ISDD'                        style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'ISDZ'                        style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'IEDD'                        style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'IEDZ'                        style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'AFRUD'                       style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'GRUND'                       style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'DURATION_NORMAL_UNIT'        style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'FIN_CONF'                    style = cond #( when status eq inativo then ativo else inativo ) )
                         ( fieldname = 'CLEAR_RES'                   style = cond #( when <opera>-fin_conf eq abap_false then inativo else ativo ) )
                      ).

      <opera>-estilo = estilo.

      if <opera>-fin_conf eq abap_false.
*        <OPERA>-CLEAR_RES = ABAP_FALSE.
      endif.

    endloop.

    loop at it_niobj assigning field-symbol(<niobj>).
      free: estilo.
      estilo = value #(
                        ( fieldname = 'TXT_OBJPTCD' style = inativo )

                        ( fieldname = 'ITEM_KEY'    style = cond #( when <niobj>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'DL_CODEGRP'  style = cond #( when <niobj>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'DL_CODE'     style = cond #( when <niobj>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'DESCRIPT'    style = cond #( when <niobj>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                      ).
      <niobj>-estilo = estilo.
    endloop.

    loop at it_nidef assigning field-symbol(<nidef>).
      free: estilo.
      estilo = value #(
                        ( fieldname = 'TXT_PROBCD' style = inativo )
                        ( fieldname = 'ITEM_KEY'   style = cond #( when <nidef>-check is not initial then status  else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'D_CODEGRP'  style = cond #( when <nidef>-check is not initial then status  else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'D_CODE'     style = cond #( when <nidef>-check is not initial then status  else cond #( when status eq inativo then ativo else inativo ) ) )
                      ).
      <nidef>-estilo = estilo.
    endloop.

    loop at it_nicau assigning field-symbol(<nicau>).
      free: estilo.
      estilo = value #(
                        ( fieldname = 'TXT_CAUSECD'   style = inativo )
                        ( fieldname = 'ITEM_KEY'      style = cond #( when <nicau>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'CAUSE_CODEGRP' style = cond #( when <nicau>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'CAUSE_CODE'    style = cond #( when <nicau>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'CAUSETEXT'     style = cond #( when <nicau>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                      ).
      <nicau>-estilo = estilo.
    endloop.

    loop at it_nitas assigning field-symbol(<nitas>).
      free: estilo.
      estilo = value #(
                        ( fieldname = 'TXT_TASKCD'   style = inativo )
                        ( fieldname = 'ITEM_KEY'     style = cond #( when <nitas>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'TASK_CODEGRP' style = cond #( when <nitas>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'TASK_CODE'    style = cond #( when <nitas>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'TASK_TEXT'    style = cond #( when <nitas>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                      ).
      <nitas>-estilo = estilo.
    endloop.

    loop at it_nifac assigning field-symbol(<nifac>).
      free: estilo.
      estilo = value #(
                        ( fieldname = 'TXT_ACTCD'   style = inativo )
                        ( fieldname = 'ITEM_KEY'    style = cond #( when <nifac>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'ACT_CODEGRP' style = cond #( when <nifac>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'ACT_CODE'    style = cond #( when <nifac>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                        ( fieldname = 'ACTTEXT'     style = cond #( when <nifac>-check is not initial then status else cond #( when status eq inativo then ativo else inativo ) ) )
                      ).
      <nifac>-estilo = estilo.
    endloop.

  endmethod.

  method set_alv1.

    me->set_fcat( 'ZOPERATIONS' ).

    if cont1 is initial.

      create object cont1
        exporting
          container_name = 'CC_01'.

      create object alv1
        exporting
          i_shellstyle    = 0
          i_parent        = cont1
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( 'CC_01' ).

      create object obj_even.

      lt_f4 =
      value #(
              ( fieldname = 'PERNR' register = abap_true getbefore = abap_true )
              ( fieldname = 'GRUND' register = abap_true getbefore = abap_true )
              ( fieldname = 'ACTIVITY' register = abap_true getbefore = abap_true )
             ).

      alv1->register_f4_for_fields( it_f4 = lt_f4[] ).

      set handler: obj_even->on_click for alv1,
                   obj_even->on_dt_ch_fs for alv1,
                   obj_even->on_onf4 for alv1,
                   obj_even->on_dt_ch for alv1.

      alv1->set_table_for_first_display(
        exporting
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        changing
*         IT_OUTTAB                     = IT_OPERA
          it_outtab                     = it_aponta
          it_fieldcatalog               = fcat[]
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4
      ).

      alv1->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      alv1->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    else.
      alv1->refresh_table_display( is_stable = stable ).
    endif.

    me->set_alv2( ).
    me->set_alv3( ).
    me->set_alv4( ).
    me->set_alv5( ).
    me->set_alv6( ).

  endmethod.

  method set_alv2.

    me->set_fcat( 'ZNOTITEMOBJ' ).

    if cont2 is initial.

      create object cont2
        exporting
          container_name = 'CC_02'.

      create object alv2
        exporting
          i_shellstyle    = 0
          i_parent        = cont2
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      create object obj_even.

      lt_f4 =
      value #(
              ( fieldname = 'DL_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'DL_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv2->register_f4_for_fields( it_f4 = lt_f4[] ).

      set handler: obj_even->on_click for alv2,
                   obj_even->on_dt_ch_fs for alv2,
                   obj_even->on_onf4 for alv2,
                   obj_even->on_dt_ch for alv2.

      alv2->set_table_for_first_display(
        exporting
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        changing
          it_outtab                     = it_niobj
          it_fieldcatalog               = fcat[]
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4
      ).

      call method alv2->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      call method alv2->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    else.
      alv2->refresh_table_display( exporting is_stable = stable ).
    endif.

  endmethod.

  method set_alv3.

    me->set_fcat( 'ZNOTITEMDEF' ).

    if cont3 is initial.

      create object cont3
        exporting
          container_name = 'CC_03'.

      create object alv3
        exporting
          i_shellstyle    = 0
          i_parent        = cont3
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      create object obj_even.

      lt_f4 =
      value #(
              ( fieldname = 'D_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'D_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv3->register_f4_for_fields( it_f4 = lt_f4[] ).

      set handler: obj_even->on_click for alv3,
                   obj_even->on_dt_ch_fs for alv3,
                   obj_even->on_onf4 for alv3,
                   obj_even->on_dt_ch for alv3.

      alv3->set_table_for_first_display(
        exporting
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        changing
          it_outtab                     = it_nidef
          it_fieldcatalog               = fcat[]
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4
      ).

      call method alv3->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      call method alv3->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    else.
      alv3->refresh_table_display( exporting is_stable = stable ).
    endif.

  endmethod.

  method set_alv4.

    me->set_fcat( 'ZNOTIFCAUS' ).

    if cont4 is initial.

      create object cont4
        exporting
          container_name = 'CC_04'.

      create object alv4
        exporting
          i_shellstyle    = 0
          i_parent        = cont4
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      create object obj_even.

      lt_f4 =
      value #(
              ( fieldname = 'CAUSE_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'CAUSE_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv4->register_f4_for_fields( it_f4 = lt_f4[] ).

      set handler: obj_even->on_click for alv4,
                   obj_even->on_dt_ch_fs for alv4,
                   obj_even->on_onf4 for alv4,
                   obj_even->on_dt_ch for alv4.

      alv4->set_table_for_first_display(
        exporting
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        changing
          it_outtab                     = it_nicau
          it_fieldcatalog               = fcat[]
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4
      ).

      call method alv4->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      call method alv4->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    else.
      alv4->refresh_table_display( exporting is_stable = stable ).
    endif.

  endmethod.

  method set_alv5.

    me->set_fcat( 'ZNOTIFTASK' ).

    if cont5 is initial.

      create object cont5
        exporting
          container_name = 'CC_05'.

      create object alv5
        exporting
          i_shellstyle    = 0
          i_parent        = cont5
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      create object obj_even.

      lt_f4 =
      value #(
              ( fieldname = 'TASK_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'TASK_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv5->register_f4_for_fields( it_f4 = lt_f4[] ).

      set handler: obj_even->on_click for alv5,
                   obj_even->on_dt_ch_fs for alv5,
                   obj_even->on_onf4 for alv5,
                   obj_even->on_dt_ch for alv5.

      alv5->set_table_for_first_display(
        exporting
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        changing
          it_outtab                     = it_nitas
          it_fieldcatalog               = fcat[]
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4
      ).

      call method alv5->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      call method alv5->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    else.
      alv5->refresh_table_display( exporting is_stable = stable ).
    endif.

  endmethod.

  method set_alv6.

    me->set_fcat( 'ZNOTIFACTV' ).

    if cont6 is initial.

      create object cont6
        exporting
          container_name = 'CC_06'.

      create object alv6
        exporting
          i_shellstyle    = 0
          i_parent        = cont6
          i_appl_events   = abap_false
          i_fcat_complete = abap_false.

      me->set_layout( ).

      create object obj_even.

      lt_f4 =
      value #(
              ( fieldname = 'ACT_CODEGRP' register = abap_true getbefore = abap_true )
              ( fieldname = 'ACT_CODE'    register = abap_true getbefore = abap_true )
             ).

      alv6->register_f4_for_fields( it_f4 = lt_f4[] ).

      set handler: obj_even->on_click for alv6,
                   obj_even->on_dt_ch_fs for alv6,
                   obj_even->on_onf4 for alv6,
                   obj_even->on_dt_ch for alv6.

      alv6->set_table_for_first_display(
        exporting
          is_layout                     = layout
          is_variant                    = variant
          i_save                        = abap_true
          it_toolbar_excluding          = it_exc
        changing
          it_outtab                     = it_nifac
          it_fieldcatalog               = fcat[]
        exceptions
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          others                        = 4
      ).

      call method alv6->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      call method alv6->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    else.
      alv6->refresh_table_display( exporting is_stable = stable ).
    endif.

  endmethod.

  method get_pernr.
    data(pernr) = it_opera[ input ]-pernr.
    select single sname from pa0001 into return where pernr eq pernr.
    it_opera[ input ]-sname = return.
  endmethod.

  method get_grund.

    data(grund) = it_opera[ input ]-grund.

    select single grdtx
      from trugt
        into return
        where grund eq grund
          and werks eq header-planplant
    and spras eq sy-langu.

    it_opera[ input ]-grdtx = return.

  endmethod.

  method get_activity.

    try .
        data(zapontat) = it_operat[ activity = input ].
      catch cx_sy_itab_line_not_found.
    endtry.

    it_opera[ input1 ]-activity    = zapontat-activity.
    it_opera[ input1 ]-description = zapontat-description.
    it_opera[ input1 ]-work_cntr   = zapontat-work_cntr.

  endmethod.

  method get_desc.

    me->get_cat_code( field = field index = input ).

    it_code = value #( for ls in codes1 where ( cat_typ    eq me->at_cat_typ and code_group eq me->at_code_group )
                         (
                           code_group = ls-code_group
                           code = ls-code
                           shorttxtcd = ls-shorttxtcd )
                         ).
    sort it_code.
    delete adjacent duplicates from it_code comparing all fields.
    me->at_code = input1.
    me->set_code( field = field index = input ).
    me->set_txt( field = field index = input ).

  endmethod.

  method f4_activity.

    it_f4activity = value #( for ls in it_operat ( corresponding #( ls ) ) ).

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = field
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = '100'
      tables
        value_tab       = it_f4activity
        return_tab      = tl_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    try.
        obj_main->get_activity(
          input  = tl_return[ 1 ]-fieldval
          input1 = index
        ).
      catch cx_sy_itab_line_not_found.
    endtry.


  endmethod.

  method refresh.
    if alv1 is not initial.
      alv1->refresh_table_display( exporting is_stable = stable ).
    endif.
    if alv2 is not initial.
      alv2->refresh_table_display( exporting is_stable = stable ).
    endif.
    if alv3 is not initial.
      alv3->refresh_table_display( exporting is_stable = stable ).
    endif.
    if alv4 is not initial.
      alv4->refresh_table_display( exporting is_stable = stable ).
    endif.
    if alv5 is not initial.
      alv5->refresh_table_display( exporting is_stable = stable ).
    endif.
    if alv6 is not initial.
      alv6->refresh_table_display( exporting is_stable = stable ).
    endif.
  endmethod.

  method help_start.

    call function 'HELP_START'
      exporting
        help_infos   = wl_help_info
      importing
        select_value = wl_selected
      tables
        dynpselect   = tl_dynpselect
        dynpvaluetab = tl_dynpvaluetab.

    if wl_selected is not initial.

      case field.
        when 'PERNR'.
          it_opera[ input ]-pernr = wl_selected.
          me->get_pernr( input ).
        when 'GRUND'.
          it_opera[ input ]-grund = wl_selected.
          me->get_grund( input ).
      endcase.

    endif.

  endmethod.

  method code_f4.

    me->get_cat_code( field = field index = input ).

    if field cs 'CODEGRP'.

      it_code_group = value #( for ls in codes1 where ( cat_typ eq me->at_cat_typ )
                                  ( code_group = ls-code_group shorttxtgr = ls-shorttxtgr )
                             ).
      sort it_code_group.
      delete adjacent duplicates from it_code_group comparing all fields.
      me->set_f4( table = it_code_group field = 'CODE_GROUP' index = input ).
      me->set_code_group( field = field index = input ).

    else.

      it_code = value #( for ls in codes1 where ( cat_typ    eq me->at_cat_typ and
                                                  code_group eq me->at_code_group
                                                )
                                  ( code_group = ls-code_group code = ls-code shorttxtcd = ls-shorttxtcd )
                       ).
      sort it_code.
      delete adjacent duplicates from it_code comparing all fields.
      me->set_f4( table = it_code field = 'CODE' index = input ).
      me->set_code( field = field index = input ).
      me->set_txt( field = field index = input ).

    endif.

    me->refresh( ).

  endmethod.

  method set_f4.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = field
        value_org       = 'S'
        dynpprog        = sy-repid
        dynpnr          = '100'
      tables
        value_tab       = table
        return_tab      = tl_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    try .
        me->at_code = tl_return[ 1 ]-fieldval.
      catch cx_sy_itab_line_not_found.
        clear me->at_code.
    endtry.


  endmethod.

  method get_cat_code.

    case field.
      when 'DL_CODE'    or 'DL_CODEGRP'   .
        me->at_cat_typ = 'B'.
        me->at_code_group = it_niobj[ index ]-dl_codegrp.
      when 'D_CODE'     or 'D_CODEGRP'    .
        me->at_cat_typ = 'C'.
        me->at_code_group = it_nidef[ index ]-d_codegrp.
      when 'CAUSE_CODE' or 'CAUSE_CODEGRP'.
        me->at_cat_typ = '5'.
        me->at_code_group = it_nicau[ index ]-cause_codegrp.
      when 'TASK_CODE'  or 'TASK_CODEGRP' .
        me->at_cat_typ = '2'.
        me->at_code_group = it_nitas[ index ]-task_codegrp.
      when 'ACT_CODE'   or 'ACT_CODEGRP'  .
        me->at_cat_typ = 'A'.
        me->at_code_group = it_nifac[ index ]-act_codegrp.
    endcase.

  endmethod.

  method set_code.
    case field.
      when 'DL_CODE'    . it_niobj[ index ]-dl_code     = me->at_code.
      when 'D_CODE'     . it_nidef[ index ]-d_code      = me->at_code.
      when 'CAUSE_CODE' . it_nicau[ index ]-cause_code  = me->at_code.
      when 'TASK_CODE'  . it_nitas[ index ]-task_code   = me->at_code.
      when 'ACT_CODE'   . it_nifac[ index ]-act_code    = me->at_code.
    endcase.
  endmethod.

  method set_code_group.
    case field.
      when 'DL_CODEGRP'    . it_niobj[ index ]-dl_codegrp     = me->at_code.
      when 'D_CODEGRP'     . it_nidef[ index ]-d_codegrp      = me->at_code.
      when 'CAUSE_CODEGRP' . it_nicau[ index ]-cause_codegrp  = me->at_code.
      when 'TASK_CODEGRP'  . it_nitas[ index ]-task_codegrp   = me->at_code.
      when 'ACT_CODEGRP'   . it_nifac[ index ]-act_codegrp    = me->at_code.
    endcase.
  endmethod.

  method set_txt.

    try .
        data(descricao) = codes1[ cat_typ = me->at_cat_typ code_group = me->at_code_group code = me->at_code ]-shorttxtcd.
      catch cx_sy_itab_line_not_found.
        clear descricao.
    endtry.

    case field.
      when 'DL_CODE'    . it_niobj[ index ]-txt_objptcd = descricao.
      when 'D_CODE'     . it_nidef[ index ]-txt_probcd  = descricao.
      when 'CAUSE_CODE' . it_nicau[ index ]-txt_causecd = descricao.
      when 'TASK_CODE'  . it_nitas[ index ]-txt_taskcd  = descricao.
      when 'ACT_CODE'   . it_nifac[ index ]-txt_actcd   = descricao.
    endcase.

  endmethod.

  method set_apontar.
* Apontamento de Atividades.

    it_timetickets =
    value #( for ls in it_opera where ( isdd is not initial and
                                        isdz is not initial and
                                        iedd is not initial and
                                        iedz is not initial )
                                (
                                  orderid         = header-orderid
                                  postg_date      = ls-budat
                                  operation       = ls-activity
                                  sub_oper        = ls-sub_activity
                                  work_cntr       = ls-work_cntr
                                  pers_no         = ls-pernr
                                  exec_start_date = ls-isdd
                                  exec_start_time = ls-isdz
                                  exec_fin_date   = ls-iedd
                                  exec_fin_time   = ls-iedz
                                  fin_conf        = ls-fin_conf
                                  act_work        = ls-afrud
                                  un_work         = ls-duration_normal_unit
                                  dev_reason      = ls-grund
                                  conf_text       = ls-ltxa1 "Ajuste referente melhoria US #174095 / AOENNING
*                                  CLEAR_RES       = LS-CLEAR_RES
                                  act_type        = ls-acttype
                                ) ).

    call function 'BAPI_ALM_CONF_CREATE'
      importing
        return        = wa_return
      tables
        timetickets   = it_timetickets
        detail_return = tg_return.

    if not line_exists( tg_return[ type = 'E' ] ).
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
      free it_opera.
      free it_aponta.

      if v_conf_enc is not initial.
        obj_main->set_encerra_ordem( ).
      endif.
    endif.


    it_return = value #( for lr in tg_return
                                            (
                                              type        = lr-type
                                              id          = lr-message_id
                                              number      = lr-message_number
                                              message     = lr-message
                                              log_no      = lr-log_number
                                              log_msg_no  = lr-log_msg_no
                                              message_v1  = lr-message_v1
                                              message_v2  = lr-message_v2
                                              message_v3  = lr-message_v3
                                              message_v4  = lr-message_v4
                                              parameter   = lr-parameter
                                              row         = lr-row
                                              field       = lr-field
                                              system      = lr-system
                                            )
                       ).

    append wa_return to it_return.
    append lines of it_return to t_return.
    free it_return.

*    Apontamento de Catalogos

*    ME->SET_NOTIFICACAO( INPUT = ABAP_TRUE ).
*
*    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
*      EXPORTING
*        NUMBER      = HEADER-NOTIF_NO
*      TABLES
*        NOTIFITEM   = TNOTIFITEM
*        NOTIFITEM_X = TNOTIFITEM_X
*        NOTIFCAUS   = TNOTIFCAUS
*        NOTIFCAUS_X = TNOTIFCAUS_X
*        NOTIFACTV   = TNOTIFACTV
*        NOTIFACTV_X = TNOTIFACTV_X
*        NOTIFTASK   = TNOTIFTASK
*        NOTIFTASK_X = TNOTIFTASK_X
*        RETURN      = IT_RETURN.
*
*    APPEND LINES OF IT_RETURN TO T_RETURN.
*
*    ME->SAVE_COMMIT( ).

    me->set_notificacao( input = abap_false ).

    call function 'BAPI_ALM_NOTIF_DATA_ADD' "#EC CI_USAGE_OK[2438131]
      exporting
        number    = header-notif_no
      tables
        notitem   = tnotifitem
        notifcaus = tnotifcaus
        notifactv = tnotifactv
*       NOTIFTASK = TNOTIFTASK
        return    = it_return.

    me->save_commit( ).

    me->set_get_return( ).
    obj_main->alv1->refresh_table_display( is_stable = obj_main->stable )."STABLE

    if v_conf_nota is not initial.
      obj_main->set_encerra_ordem_nota( ).
    endif.

    leave to current transaction.

  endmethod.

  method set_encerra_ordem.

    perform check_req_and_ped_pendente using header-orderid.

    if t_req_pend is not initial or
       t_ped_pend is not initial.
      message i007 with header-orderid display like 'E'.
    else.


      data: equipment  type equnr,
            standorder type daufn,
            settlorder type ilom_ordst.

      clear: it_methods, wa_return.

      data: lv_refnum  type ifrefnum.

      wa_methods-refnumber = 1.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'TECHNICALCOMPLETE '.
      wa_methods-objectkey = header-orderid.
      append wa_methods to it_methods.

      wa_methods-objecttype = ''.
      wa_methods-method = 'SAVE'.
      append wa_methods to it_methods.

      wa_header-orderid = header-orderid.
      append wa_header to it_header.


      call function 'BAPI_ALM_ORDER_MAINTAIN' "#EC CI_USAGE_OK[2669857]
        tables                                "#EC CI_USAGE_OK[2438131]
          it_methods = it_methods
          it_header  = it_header
          return     = it_return.

      append lines of it_return to t_return.
      if not line_exists( it_return[ type = 'E' ] ).
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait   = abap_true
          importing
            return = wa_return.
      endif.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        WAIT   = 'X'
*      IMPORTING
*        RETURN = WA_RETURN.

      me->set_get_return( ).
    endif.

  endmethod.

  method set_encerra_ordem_nota.
    data: wa_return type bapiret2.

    call function 'BAPI_ALM_NOTIF_CLOSE'
      exporting
        number       = ld_number-notif_no
        syststat     = ld_syststat
        testrun      = ld_testrun
      importing
        systemstatus = ld_systemstatus-systatus
        userstatus   = ld_systemstatus-usrstatus
      tables
        return       = it_return.  "  BAPI_ALM_NOTIF_CLOSE

    if it_return is initial.
      message s408(im) with ld_number-notif_no.

      append lines of it_return to t_return.
      me->set_get_return( ).

      me->save_commit( ).
    else.
      append lines of it_return to t_return.
      me->set_get_return( ).

*      LOOP AT IT_RETURN INTO WA_RETURN.
*        MESSAGE E000(O0) WITH WA_RETURN-MESSAGE.
*      ENDLOOP.
    endif.

  endmethod.

  method set_edit_nota.
    me->refresh( ).
  endmethod.

  method set_save_nota.

    me->set_notificacao( input = abap_true ).

    call function 'BAPI_ALM_NOTIF_DATA_MODIFY' "#EC CI_USAGE_OK[2438131]
      exporting
        number      = header-notif_no
      tables
*       notifitem   = tnotifXitem
        notifitem_x = tnotifitem_x
        notifcaus   = tnotifcaus
        notifcaus_x = tnotifcaus_x
        notifactv   = tnotifactv
        notifactv_x = tnotifactv_x
        notiftask   = tnotiftask
        notiftask_x = tnotiftask_x
        return      = it_return.

    append lines of it_return to t_return.

    me->save_commit( ).

    v_ordem = |{ v_ordem alpha = in }|.
    obj_main->set_estrutura( v_ordem ).

  endmethod.

  method set_get_return.

    check t_return  is not initial.

    call function 'FINB_BAPIRET2_DISPLAY'
      exporting
        it_message = t_return.

    call function 'Z_GRAVA_LOG_PM'
      tables
        t_return = t_return.

    free t_return.

  endmethod.

  method add_line.

    if reduce i( init x = 0 for wa1 in it_opera where ( activity is initial ) next x = x + 1 ) is initial.
      append initial line to it_opera.
    endif.

    if reduce i( init x = 0 for wa2 in it_niobj where ( dl_codegrp is initial ) next x = x + 1 ) is initial.
      append initial line to it_niobj.
    endif.

    if reduce i( init x = 0 for wa3 in it_nidef where ( d_codegrp is initial ) next x = x + 1 ) is initial.
      append initial line to it_nidef.
    endif.

    if reduce i( init x = 0 for wa4 in it_nicau where ( cause_codegrp is initial ) next x = x + 1 ) is initial.
      append initial line to it_nicau.
    endif.

    if reduce i( init x = 0 for wa5 in it_nitas where ( task_codegrp is initial ) next x = x + 1 ) is initial.
      append initial line to it_nitas.
    endif.

    if reduce i( init x = 0 for wa6 in it_nifac where ( act_codegrp is initial ) next x = x + 1 ) is initial.
      append initial line to it_nifac.
    endif.

    me->set_block( ).

  endmethod.

  method set_notificacao.

    free: tnotifitem, tnotifcaus, tnotifactv, tnotiftask.
    free: tnotifitem_x, tnotifcaus_x, tnotifactv_x, tnotiftask_x.

    tnotifitem   = value #( for l1 in it_niobj where ( check eq input and item_key is not initial )
                            (
                              item_key     = l1-item_key
                              item_sort_no = l1-item_key"COND #( WHEN INPUT EQ ABAP_TRUE THEN L1-ITEM_SORT_NO ELSE L1-ITEM_KEY )
                              descript     = l1-descript
                              dl_codegrp   = l1-dl_codegrp
                              dl_code      = l1-dl_code
                            ) ).

    loop at tnotifitem assigning field-symbol(<item>).

      try .
          data(wa_nidef) = it_nidef[ item_key     = <item>-item_key ].
        catch cx_sy_itab_line_not_found.
      endtry.

      <item>-d_codegrp = wa_nidef-d_codegrp.
      <item>-d_code    = wa_nidef-d_code.

    endloop.

    tnotifitem_x = value #( for l1 in it_niobj where ( check eq input and item_key is not initial )
                            (
                              item_key     = l1-item_key
                              item_sort_no = abap_true
                              descript     = abap_true
                              d_codegrp    = abap_true
                              d_code       = abap_true
                              dl_codegrp   = abap_true
                              dl_code      = abap_true
                            ) ).

    tnotifcaus   = value #( for l2 in it_nicau where ( check eq input and item_key is not initial )
                            (
                              cause_key       = l2-cause_key"'0001'
                              cause_sort_no   = l2-item_key"'0001'
                              item_key        = l2-item_key
                              causetext       = l2-causetext
                              cause_codegrp   = l2-cause_codegrp
                              cause_code      = l2-cause_code
                              item_sort_no    = l2-item_key
                            ) ).

    tnotifcaus_x = value #( for l2 in it_nicau where ( check eq input and item_key is not initial )
                            (
                              cause_key       = l2-cause_key
                              cause_sort_no   = abap_true
                              item_key        = l2-item_key
                              causetext       = abap_true
                              cause_codegrp   = abap_true
                              cause_code      = abap_true
                              item_sort_no    = l2-item_key
                             ) ).

    tnotifactv   = value #( for l3 in it_nifac where ( check eq input and act_key is not initial )
                            (
                              act_key      = l3-act_key
                              act_sort_no  = l3-act_key"ACT_SORT_NO
                              acttext      = l3-acttext
                              act_codegrp  = l3-act_codegrp
                              act_code     = l3-act_code
                             ) ).

    tnotifactv_x = value #( for l3 in it_nifac where ( check eq input and act_key is not initial )
                             (
                              act_key      = l3-act_key
                              act_sort_no  = abap_true
                              acttext      = abap_true
                              act_codegrp  = abap_true
                              act_code     = abap_true
                             ) ).

    tnotiftask   = value #( for l4 in it_nitas where ( check eq input and task_key is not initial )
                            (
                              task_key      = l4-task_key
                              task_sort_no  = l4-task_key"TASK_SORT_NO
                              task_text     = l4-task_text
                              task_codegrp  = l4-task_codegrp
                              task_code     = l4-task_code
                            ) ).

    tnotiftask_x = value #( for l4 in it_nitas where ( check eq input and task_key is not initial )
                            (
                              task_key      = l4-task_key
                              task_sort_no  = abap_true
                              task_text     = abap_true
                              task_codegrp  = abap_true
                              task_code     = abap_true
                            ) ).

  endmethod.

  method
    save_commit.

    call function 'ALM_PM_NOTIFICATION_SAVE' "#EC CI_USAGE_OK[2438006]
      exporting
        number = ld_number-notif_no
      tables
        return = it_return.


    append lines of it_return to t_return.

    if not line_exists( it_return[ type = 'E' ] ).
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    endif.

  endmethod.

endclass.

start-of-selection.

  obj_main->screen( ).
