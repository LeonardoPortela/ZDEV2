*&---------------------------------------------------------------------*
*&  Include           ZGL034_CLASS
*&---------------------------------------------------------------------*

class lcl_alv_toolbar_0100 implementation.
  method constructor.
*   Create ALV toolbar manager instance
    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.                    "constructor

  method on_toolbar.

    clear ty_toolbar.
    ty_toolbar-icon      = icon_create.
    ty_toolbar-function  = 'INSR'.
    ty_toolbar-text      = 'Incluir Lcto.'.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-icon      = icon_display.
    ty_toolbar-function  = 'DISPLAY'.
    ty_toolbar-text      = 'Visualizar'.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-icon      = icon_change.
    ty_toolbar-function  = 'EDIT'.
    ty_toolbar-text      = 'Modificar'.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'DELE'.
    ty_toolbar-text      = 'Eliminar Lcto.'.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-icon      = icon_wizard.
    ty_toolbar-function  = 'GER_CTB'.
    ty_toolbar-text      = 'Gerar Ctb.'.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

*    TY_TOOLBAR-ICON      = ICON_REFRESH.
*    TY_TOOLBAR-FUNCTION  = 'REFRESH_CTB'.
*    TY_TOOLBAR-TEXT      = 'Atualizar'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    call method c_alv_toolbarmanager->reorganize
      exporting
        io_alv_toolbar = e_object.

  endmethod.                    "on_toolbar

  method handle_user_command.

    case e_ucomm.
      when 'GER_CTB'.

        perform valida_data_usuario_permissao. "115706 CS2022000452 Bloquear Geração de Doc. Fiscal em Período Fechado - ZGL059 - PSA
        if msg_erro is not initial.
          message msg_erro type 'I' display like 'E'.
        else.
          perform f_gerar_ctb_sel.
        endif.

      when 'INSR'.
        perform f_novo_lcto.
      when 'DISPLAY'.
        perform f_display_lcto.
      when 'EDIT'.
        perform f_edit_lcto.
      when 'DELE'.
        perform f_eliminar_lcto.
      when 'REFRESH_CTB'.
        perform f_refresh_ctb.
    endcase.

  endmethod.                    "HANDLE_USER_COMMAND

endclass.                    "lcl_alv_toolbar IMPLEMENTATION

class lcl_alv_toolbar_0110 implementation.
  method constructor.
*   Create ALV toolbar manager instance
    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.                    "constructor

  method on_toolbar.

    if vg_opr_lcto ne c_display.

      clear ty_toolbar.
      ty_toolbar-icon        = icon_import.
      ty_toolbar-function    = 'IMPORT'.
      ty_toolbar-text        = 'Importar'.
      ty_toolbar-butn_type   = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      clear ty_toolbar.
      ty_toolbar-icon        = icon_intensify_critical.
      ty_toolbar-function    = 'APLIC_TXT_CTB'.
      ty_toolbar-text        = 'Aplicar Texto Ctb.'.
      ty_toolbar-butn_type   = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      call method c_alv_toolbarmanager->reorganize
        exporting
          io_alv_toolbar = e_object.

    else.

      ty_toolbar-function     = 'GER_CTB'.
      ty_toolbar-icon         = icon_wizard.
      ty_toolbar-butn_type    = 0.
      ty_toolbar-text         = 'Gerar Ctb.'.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-function     = 'ESTORNO_CTB'.
      ty_toolbar-icon         = icon_storno.
      ty_toolbar-butn_type    = 0.
      ty_toolbar-text         = 'Estornar Ctb.'.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-function     = 'GER_NF'.
      ty_toolbar-icon         = icon_document.
      ty_toolbar-butn_type    = 0.
      ty_toolbar-text         = 'Gerar NF'.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-function     = 'ESTORNO_NF'.
      ty_toolbar-icon         = icon_storno.
      ty_toolbar-butn_type    = 0.
      ty_toolbar-text         = 'Estornar NF.'.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-icon      = icon_refresh.
      ty_toolbar-function  = 'REFRESH_NF'.
      ty_toolbar-text      = 'Atualizar'.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.


    endif.


  endmethod.                    "on_toolbar

  method handle_user_command.

    perform valida_data_usuario_permissao. "115706 CS2022000452 Bloquear Geração de Doc. Fiscal em Período Fechado - ZGL059 - PSA
    if msg_erro is not initial.
      message msg_erro type 'I' display like 'E'.
    else.

      case e_ucomm.
        when 'APLIC_TXT_CTB'.
          perform f_aplic_txt_ctb.
        when 'IMPORT'.
          perform f_importar_lcto.
        when 'GER_CTB'.
          perform: f_gerar_ctb_lcto using wa_saida_0100,
                   f_seleciona_0110 using wa_saida_0100,
                   f_refresh_alv    using '0110'.
        when 'ESTORNO_CTB'.
          perform f_estorno_ctb.
        when 'GER_NF'.
          perform f_gerar_nf_sel.
        when 'REFRESH_NF'.
          perform f_seleciona_0110 using wa_saida_0100.
          perform f_refresh_alv using '0110'.
        when 'ESTORNO_NF'.
          perform f_estorno_nf.
      endcase.

    endif.

  endmethod.                    "HANDLE_USER_COMMAND

endclass.                    "lcl_alv_toolbar IMPLEMENTATION

class lcl_event_handler_0100 implementation.
  method handle_hotspot_click.
    perform f_handle_hotspot_click using e_row_id e_column_id es_row_no.
  endmethod.
endclass.

class lcl_event_handler_0110 implementation.

  method on_data_changed_finished.

    data: ls_good      type lvc_s_modi.

    if et_good_cells[] is not initial.

      read table et_good_cells into ls_good index 1.

      read table it_saida_0110 into wa_saida_0110 index ls_good-row_id.
      if sy-subrc = 0.
        perform f_conversion_input_0110 using wa_saida_0110.
        modify it_saida_0110 from wa_saida_0110 index ls_good-row_id.
      endif.

    endif.

    call method obj_alv_0110->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "on_data_changed_finishe

  method catch_hotspot.

    data: gt_086  type table of zglt086,
          vl_lote type zglt034-lote.

    case e_column_id.
      when 'STATUS_NF'.
        read table it_saida_0110 into wa_saida_0110 index e_row_id-index.
        check sy-subrc = 0.

        if wa_saida_0110-status_nf = icon_red_light.
          refresh: gt_086.
          select *
            from zglt086 into table gt_086
           where seq_lcto = wa_saida_0110-seq_lcto
             and agrp_nf  = wa_saida_0110-agrp_nf.

          if gt_086[] is not initial.
            perform f_montar_layout.
            call function 'REUSE_ALV_GRID_DISPLAY'
              exporting
                it_fieldcat           = estrutura[]
                i_save                = 'A'
                i_screen_start_column = 3
                i_screen_start_line   = 3
                i_screen_end_column   = 100
                i_screen_end_line     = 13
              tables
                t_outtab              = gt_086.
          endif.
        endif.
      when 'DOCNUM'.

        read table it_saida_0110 into wa_saida_0110 index e_row_id-index.
        check sy-subrc = 0.
        check wa_saida_0110-docnum is not initial.
        set parameter id 'JEF' field wa_saida_0110-docnum.
        call transaction 'J1B3N' and skip first screen.

      when 'DOC_LCTO'.
        clear: wa_saida_0110.
        read table it_saida_0110 into wa_saida_0110 index e_row_id-index.

        check wa_saida_0110-doc_lcto is not initial.

        clear: vl_lote.
        set parameter id 'BLN' field wa_saida_0110-doc_lcto.
        set parameter id 'LOT' field vl_lote.
        call transaction 'ZGL016' and skip first screen.
      when 'BELNR'.
        clear: wa_saida_0110.
        read table it_saida_0110 into wa_saida_0110 index e_row_id-index.

        check wa_saida_0110-belnr is not initial.

        set parameter id 'BLN' field wa_saida_0110-belnr.
        set parameter id 'BUK' field wa_saida_0110-bukrs.
        set parameter id 'GJR' field wa_saida_0110-dt_lcto_ctb(4).
        call transaction 'FB03' and skip first screen.



    endcase.



  endmethod.

  method on_onf4.

    types: begin of ty_field,
             tabname   type dd03l-tabname,
             fieldname type dd03l-fieldname,
             s(1)      type c,
           end of ty_field,

           begin of ty_value,
             tabname    type dd03l-tabname,
             fieldname  type dd03l-fieldname,
             char79(79) type c,
           end of ty_value,

           begin of ty_asmd,
             asnum type asmd-asnum,
             asktx type asmdt-asktx,
           end of ty_asmd,

           begin of ty_csks,
             kokrs type csks-kokrs,
             kostl type csks-kostl,
             datbi type csks-datbi,
             datab type csks-datab,
             ltext type cskt-ltext,
           end of ty_csks.

    data: begin of wl_valuetab,
            field(50),
          end of wl_valuetab.

    "Tabelas internas
    data: gt_j_1bbranch type table of j_1bbranch,
          gt_csks       type table of ty_csks,
          gt_asmd       type table of ty_asmd,
          gt_valuetab   like table of wl_valuetab,
          gt_field      type table of ty_field,
          gt_value      type table of ty_value.

    " Tabelas work-áreas
    data: wl_j_1bbranch type j_1bbranch,
          wl_csks       type ty_csks,
          wl_asmd       type ty_asmd,
          wl_field      type ty_field,
          wl_value      type ty_value.

    " Variáveis
    data: wl_char(20),
          wl_fieldname(30),
          wl_tabname(30),
          wl_index         type sy-tabix,
          v_anln1          type anln1,
          lv_estado        type zaa001-cod_regi.

    define d_preenche_value.
      wl_valuetab = &1.
      append wl_valuetab to gt_valuetab.
    end-of-definition.

    case e_fieldname.
      when 'GSBER'.

        read table it_saida_0110 into wa_saida_0110 index es_row_no-row_id.

        if ( zglt080-bukrs is initial ).
          message s836(sd) with text-e04 display like 'E'.
          return.
        endif.

        select distinct branch name
          from j_1bbranch
          into corresponding fields of table gt_j_1bbranch
         where bukrs eq zglt080-bukrs.

        wl_fieldname = 'BRANCH'.
        wl_tabname   = 'J_1BBRANCH'.

        loop at gt_j_1bbranch into wl_j_1bbranch.
          d_preenche_value: wl_j_1bbranch-branch,
                            wl_j_1bbranch-name.
        endloop.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'BRANCH'.
        wl_field-s         = 'X'.
        append wl_field to gt_field.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'NAME'.
        wl_field-s         = 'X'.
        append wl_field to gt_field.

      when 'KOSTL'.

        read table it_saida_0110 into wa_saida_0110 index es_row_no-row_id.

        if ( zglt080-bukrs is initial ).
          message s836(sd) with text-e04 display like 'E'.
          return.
        endif.

        if ( wa_saida_0110-gsber is initial ).
          message s836(sd) with text-e05 display like 'E'.
          return.
        endif.

        select a~kokrs a~kostl a~datbi a~datab b~ltext
          into corresponding fields of table gt_csks
          from csks as a inner join cskt as b on a~kokrs = b~kokrs
                                             and a~kostl = b~kostl
                                             and a~datbi = b~datbi
         where a~bukrs  = zglt080-bukrs
           and a~gsber  = wa_saida_0110-gsber
           and a~datab  le sy-datum
           and a~datbi  ge sy-datum
           and b~spras  eq sy-langu.

        wl_fieldname = 'KOSTL'.
        wl_tabname   = 'CSKS'.

        loop at gt_csks into wl_csks.
          move wl_csks-kostl to wl_valuetab-field.
          append wl_valuetab to gt_valuetab.

          move wl_csks-ltext to wl_valuetab-field.
          append wl_valuetab to gt_valuetab.
        endloop.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'KOSTL'.
        wl_field-s         = 'X'.
        append wl_field to gt_field.

        wl_field-tabname   = 'CSKT'.
        wl_field-fieldname = 'LTEXT'.
        wl_field-s         = 'X'.
        append wl_field to gt_field.

      when 'ASNUM'.

*        SELECT A~ASNUM B~ASKTX
*          INTO CORRESPONDING FIELDS OF TABLE GT_ASMD
*          FROM ASMD AS A INNER JOIN ASMDT AS B ON A~ASNUM = B~ASNUM.
*
*        SORT GT_ASMD BY ASNUM.
*        LOOP AT GT_ASMD INTO WL_ASMD.
*          MOVE WL_ASMD-ASNUM TO WL_VALUETAB-FIELD.
*          APPEND WL_VALUETAB TO GT_VALUETAB.
*
*          MOVE WL_ASMD-ASKTX TO WL_VALUETAB-FIELD.
*          APPEND WL_VALUETAB TO GT_VALUETAB.
*        ENDLOOP.
*
*        WL_FIELD-TABNAME   = 'ASMD'.
*        WL_FIELD-FIELDNAME = 'ASNUM'.
*        WL_FIELD-S         = 'X'.
*        APPEND WL_FIELD TO GT_FIELD.
*
*        WL_FIELD-TABNAME   = 'ASMDT'.
*        WL_FIELD-FIELDNAME = 'ASKTX'.
*        WL_FIELD-S         = 'X'.
*        APPEND WL_FIELD TO GT_FIELD.


      when 'ID_LMS'.
        types: ty_range_lms type range of zmmt0104-c_custo.
        clear: wa_saida_0110.

        read table it_saida_0110 into wa_saida_0110 index es_row_no-row_id.
        if wa_saida_0110-kostl is not initial.
          data(r_lms) = value ty_range_lms( (
              sign   = 'I'
              option = 'EQ'
              low    = wa_saida_0110-kostl ) ).
        endif.

        select z104~*
          from zmmt0104 as z104
          into table @data(it_zmmt0104).
        "WHERE Z104~C_CUSTO IN @R_LMS. Removido filtro de centro de custo IR037908

        if sy-subrc = 0.

          loop at it_zmmt0104 into data(w_zmmt0104).
            move w_zmmt0104-cpf to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-id_lms to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-nome_curso to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-c_custo to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-id_turma to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-tp_solicitacao to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-data to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-hora to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
            move w_zmmt0104-usuario to wl_valuetab-field.
            append wl_valuetab to gt_valuetab.
          endloop.


          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'CPF'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'ID_LMS'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'NOME_CURSO'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'C_CUSTO'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'ID_TURMA'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'TP_SOLICITACAO'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'DATA'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'HORA'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

          wl_field-tabname   = 'ZMMT0104'.
          wl_field-fieldname = 'USUARIO'.
          wl_field-s         = 'X'.
          append wl_field to gt_field.

        endif.

    endcase.

    call function 'HELP_VALUES_GET_WITH_TABLE_EXT'
      exporting
        cucol                     = '10'
        curow                     = '5'
        fieldname                 = wl_fieldname
        tabname                   = wl_tabname
      importing
        index                     = wl_index
        select_value              = wl_char
      tables
        fields                    = gt_field
        select_values             = gt_value
        valuetab                  = gt_valuetab
      exceptions
        field_not_in_ddic         = 001
        more_then_one_selectfield = 002
        no_selectfield            = 003.

    case e_fieldname.
      when 'GSBER'.
        read table gt_value into wl_value with key fieldname = 'BRANCH'.
        if sy-subrc = 0.
          move: wl_value-char79 to wa_saida_0110-gsber.
          modify it_saida_0110 from wa_saida_0110 index es_row_no-row_id
            transporting gsber.
        endif.
      when 'KOSTL'.
        read table gt_value into wl_value with key fieldname = 'KOSTL'.
        if sy-subrc = 0.
          move: wl_value-char79 to wa_saida_0110-kostl.
          modify it_saida_0110 from wa_saida_0110 index es_row_no-row_id
            transporting kostl.
        endif.
      when 'ASNUM'.
        read table gt_value into wl_value with key fieldname = 'ASNUM'.
        if sy-subrc = 0.
          move: wl_value-char79 to wa_saida_0110-asnum.
          modify it_saida_0110 from wa_saida_0110 index es_row_no-row_id
            transporting asnum.
        endif.
      when 'ID_LMS'.
        read table gt_value into wl_value with key fieldname = 'ID_LMS'.
        if sy-subrc = 0.
          move: wl_value-char79 to wa_saida_0110-id_lms.
          modify it_saida_0110 from wa_saida_0110 index es_row_no-row_id
            transporting id_lms.

          try.
              data(w_treina) = value ty_saida_0110_treina(
                seq_lcto = wa_saida_0110-seq_lcto
                seqitem  = wa_saida_0110-seqitem
                cpf      = gt_value[ fieldname = 'CPF' ]-char79
                id_lms   = wa_saida_0110-id_lms          ).
              append w_treina to it_0110_treina[].
            catch cx_sy_itab_line_not_found.
          endtry.

        endif.

    endcase.

    call method obj_alv_0110->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "ON_ONF4


endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION

form valida_data_usuario_permissao.

  data: aux_user            type zfit0033,
        v_monat             type zfit0033-monat,
        v_gjahr             type zfit0033-gjahr,
        v_usnam             type zfit0033-usnam,
        v_resposta(8)       type c,
        v_datum             type sy-datum,
        v_budat             type budat,
        v_mes_ano_atual(10) type c,
        v_mes_ano_lanc(10)  type c,
        v_bukrs             type  bukrs.

  data: WS_saida_0110 type ty_saida_0110.

  clear: msg_erro.

*  CONDENSE p_budat NO-GAPS.

*&--------Inicio ajuste STEFANINI - FT - IR187181 - ZGL059---Código comentado&
*  v_budat = p_budat.
*&--------Fim ajuste STEFANINI - FT - IR187181 - ZGL059---Código comentado&

  v_bukrs = zglt080-bukrs."p_bukrs-low.
*  v_monat = p_budat+4(2).
*  v_gjahr = p_budat+0(4).
  v_usnam = sy-uname. " p_usnam-low.
  v_datum = sy-datum.


  field-symbols: <saida_0110> type ty_saida_0110.

  clear: it_sel_rows[], wa_sel_rows.

  call method obj_alv_0110->get_selected_rows
    importing
      et_index_rows = it_sel_rows.

  check it_sel_rows is not initial.

  if lines( it_sel_rows[] ) >  1.
    message s836(sd) with text-e19 display like 'W'.
    return.
  endif.

  loop at it_sel_rows into wa_sel_rows.

    read table it_saida_0110 into ws_saida_0110 index wa_sel_rows-index.
    check sy-subrc = 0.

    "Separar data de lançamento.
    v_monat = |{ ws_saida_0110-budat+4(2) }|.
    v_gjahr = |{ ws_saida_0110-budat+0(4) }|.
    v_budat = ws_saida_0110-budat.
  endloop.


**********************************************************************
* Verifica Usuário Bloqeado/Liberado
**********************************************************************
  select single *
      from zfit0033
      into aux_user
      where monat      = v_monat
      and   gjahr      = v_gjahr
      and   usnam      = v_usnam.
**********************************************************************
* Verifica Periodo Aberto/Fechado
**********************************************************************
  if v_budat is not initial and v_bukrs is not initial.
    call function 'Z_RET_DATA_MES_ABERTO'
      exporting
        p_data_ent  = v_budat
        p_bukrs     = v_bukrs
      importing
        p_data_val  = v_datum
      exceptions
        sem_periodo = 1
        others      = 2.
  endif.
**********************************************************************

  if  v_datum+0(6) ne v_budat+0(6) and v_budat+4(2) le 12.
    data(msg_periodo_fechado) = 'Período Fechado!'. "msg-e49
  endif.

*----------Inicio ajuste referente BUG SOLTO #146304 / AOENNING-----
  v_mes_ano_atual = sy-datum+0(06).
  v_mes_ano_lanc = ws_saida_0110-budat+0(06).


  if v_mes_ano_lanc < v_mes_ano_atual.
    if aux_user-lib_contab is initial.
      data(msg_user_bloqueado) = 'Usuário bloqueado para lançamento'.
    endif.
  endif.
*----------Fim ajuste referente BUG SOLTO #146304 / AOENNING-----

  if msg_periodo_fechado is not initial or msg_user_bloqueado is not initial.


    if msg_user_bloqueado is not initial and msg_periodo_fechado is not initial .

      msg_erro = |{ msg_user_bloqueado } e { msg_periodo_fechado }| .

    else.

      if msg_user_bloqueado is not initial.

        msg_erro = msg_user_bloqueado.

      elseif msg_periodo_fechado is not initial.
        msg_erro = msg_periodo_fechado.
      endif.

    endif.



  endif.

endform.
