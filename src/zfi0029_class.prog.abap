*&---------------------------------------------------------------------*
*&  Include           ZFI0029_CLASS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS LCL_UTILS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_utils definition.
  public section.
    methods seleciona_moeda importing
                               i_moeda1          type waers
                               i_moeda2          type waers
                               i_data            type datum
                            exporting
                                e_taxa           type wkurs.

    methods concatenar_objkey importing
                               i_belnr           type bkpf-belnr
                               i_gjahr           type gjahr
                               i_seq             type c
                               i_estor           type c
                           exporting
                              e_objkey           type awkey.


    methods z_style_disable_edit importing
                              fieldname          type any
                              style              type any.

    methods calcular_diferenca importing
                               i_belnr           type bkpf-belnr
                               exporting
                                e_diferenca      type dmbtr
                                e_maior_negativo type dmbtr
                                e_maior_positivo type dmbtr.

  private section.

    data: at_maior_negativo type dmbtr,
          at_maior_positivo type dmbtr,
          at_positivo       type dmbtr,
          at_negativo       type dmbtr,
          at_0              type dmbtr.

endclass.                    "LCL_UTILS DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_UTILS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_utils implementation.

  method calcular_diferenca.

    clear: at_maior_negativo,
           at_maior_positivo,
           at_positivo,
           at_negativo.

    data: wl_saida_aux type ty_saida.

    loop at gt_saida into wl_saida_aux where check = 'X'
                                         and belnr = i_belnr.

      if ( wl_saida_aux-vlr_ajus < at_0 ).

        if wl_saida_aux-vlr_ajus < at_maior_negativo.
          at_maior_negativo = wl_saida_aux-vlr_ajus.
        endif.

        at_negativo = at_negativo + wl_saida_aux-vlr_ajus.

      else.

        if wl_saida_aux-vlr_ajus > at_maior_positivo.
          at_maior_positivo = wl_saida_aux-vlr_ajus.
        endif.

        at_positivo = at_positivo + wl_saida_aux-vlr_ajus.
      endif.
    endloop.

    e_maior_negativo = at_maior_negativo.
    e_maior_positivo = at_maior_positivo.
    e_diferenca      = at_negativo + at_positivo.

  endmethod.                    "CALCULAR_DIFERENCA

*----------------------------------------------------------*
* Descrição: Método seleciona taxa da moeda que for passado.
* Importing:
* I_MOEDA1 = Recebe moeda de procedência.
* I_MOEDA2 = Recebe moeda de destino.
* I_DATA   = Data a partir da qual o câmbio é válido.
* Exporting:
* E_TAXA   = Retorna a taxa.
*----------------------------------------------------------*

  method seleciona_moeda.
    data: at_data_formated  type char10,
          at_data_converted type char10.

    call function 'CONVERSION_EXIT_PDATE_OUTPUT'
      exporting
        input  = i_data
      importing
        output = at_data_converted.

    call function 'CONVERSION_EXIT_INVDT_INPUT'
      exporting
        input  = at_data_converted
      importing
        output = at_data_formated.

    select single *
      from tcurr
      into wl_tcurr
     where kurst = 'B'
       and fcurr = i_moeda1
       and tcurr = i_moeda2
       and gdatu = at_data_formated.

    if i_moeda1 = c_brl.
      wl_tcurr-ukurs = wl_tcurr-ukurs * -1.
    endif.

*   Pega a taxa do dolar referênte a data informada.
    if ( sy-subrc is initial ).
      e_taxa = wl_tcurr-ukurs.
    else.
      e_taxa = '3.787'.
    endif.

  endmethod.                    "SELECIONA_MOEDA

  method concatenar_objkey.
    clear e_objkey.

    if i_estor is initial.
      concatenate 'ACD' i_belnr i_gjahr into e_objkey.
    else.
      concatenate 'ACD' i_belnr i_seq i_gjahr into e_objkey.
    endif.

  endmethod.                    "CONCATENAR_OBJKEY

  method z_style_disable_edit.

    wl_estilo-fieldname = fieldname.
    wl_estilo-style     = style.

    append wl_estilo to gt_estilo.
  endmethod.                    "Z_STYLE_DISABLE_EDIT
endclass.                    "LCL_UTILS IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.
    class-methods:
    on_data_changed for event data_changed of cl_gui_alv_grid
                        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    class-methods:
    on_click for event hotspot_click  of cl_gui_alv_grid
                        importing e_row_id e_column_id es_row_no.
endclass.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler implementation.
  method on_data_changed.
    data: ls_good type lvc_s_modi.

    loop at er_data_changed->mt_good_cells into ls_good.
      read table gt_saida into wl_saida index ls_good-row_id.

      case ls_good-fieldname.
        when 'CHECK'.
*          WL_SAIDA-CHECK = LS_GOOD-VALUE.

          if ls_good-value = 'X'.
            loop at gt_saida into wl_saida where belnr  = wl_saida-belnr
                                             and status = icon_light_out.

              wl_saida-check = ls_good-value.

              modify gt_saida from wl_saida index sy-tabix.
            endloop.
          else.
            wl_saida-check  = ls_good-value.
            modify gt_saida from wl_saida index ls_good-row_id.
          endif.

          call method obj_alv_0100->refresh_table_display
            exporting
              is_stable = wl_stable.

        when 'TX_AJUSTE'.
          wl_saida-tx_ajuste = ls_good-value.

          try.
              wl_saida-vlr_calc = ( wl_saida-dmbtr_brl / wl_saida-tx_ajuste ).
            catch cx_sy_zerodivide.
          endtry.

          wl_saida-vlr_ajus  = ( wl_saida-vlr_calc  - wl_saida-dmbtr_usd ).

          modify gt_saida from wl_saida index ls_good-row_id
          transporting tx_ajuste vlr_calc vlr_ajus.

          call method obj_alv_0100->refresh_table_display
            exporting
              is_stable = wl_stable.

        when 'VLR_AJUS'.

          data: num type dmbtr.

          call function 'MOVE_CHAR_TO_NUM'
            exporting
              chr = ls_good-value
            importing
              num = num.

          wl_saida-vlr_ajus = num.

          modify gt_saida from wl_saida index ls_good-row_id
          transporting vlr_ajus.

          call method obj_alv_0100->refresh_table_display
            exporting
              is_stable = wl_stable.
      endcase.
    endloop.
  endmethod.                    "ON_DATA_CHANGED

  method on_click.
    data: r_utils type ref to lcl_utils,
          v_ans   type c,
          v_mode  type c,
          v_index type sy-tabix.

    create object r_utils.

    read table gt_saida into wl_saida index e_row_id.
    clear: gt_estilo[], wl_saida-estilo, v_ans, gt_bdc.

    case e_column_id.
      when 'BELNR'.
        set parameter id 'BLN' field wl_saida-belnr.
        set parameter id 'BUK' field wl_saida-bukrs.
        set parameter id 'GJR' field wl_saida-gjahr.

        call transaction 'FB03' and skip first screen.

      when 'OBJKEY'.
        case wl_saida-status.
          when icon_red_light.

            perform f_preencher_dynpro using:
            'X' 'SAPLSETB'                    '0230',
            ' ' 'DATABROWSE-TABLENAME'        'ZIB_CONTABIL_ERR',
            ' ' 'BDC_OKCODE'                  '=ANZE',
            'X'  '/1BCDWB/DBZIB_CONTABIL_ERR' '1000',
            ' '  'I1-LOW'                      wl_saida-objkey,
            ' '  'BDC_OKCODE'                  '=ONLI'.

            opt-dismode  = 'E'.
            opt-updmode  = 'A'.
            call transaction 'SE16' using gt_bdc options from opt.

          when icon_green_light.

            perform f_preencher_dynpro using:
            'X' 'SAPLSETB'                    '0230',
            ' ' 'DATABROWSE-TABLENAME'        'ZIB_CONTABIL_CHV',
            ' ' 'BDC_OKCODE'                  '=ANZE',
            'X'  '/1BCDWB/DBZIB_CONTABIL_CHV' '1000',
            ' '  'I1-LOW'                      wl_saida-objkey,
            ' '  'BDC_OKCODE'                 '=ONLI'.

            opt-dismode  = 'E'.
            opt-updmode  = 'A'.
            call transaction 'SE16' using gt_bdc options from opt.

          when icon_storno.

            perform f_preencher_dynpro using:
            'X' 'SAPLSETB'                    '0230',
            ' ' 'DATABROWSE-TABLENAME'        'ZIB_CONTABIL_ERR',
            ' ' 'BDC_OKCODE'                  '=ANZE',
            'X'  '/1BCDWB/DBZIB_CONTABIL_ERR' '1000',
            ' '  'I1-LOW'                      wl_saida-objkey,
            ' '  'BDC_OKCODE'                 '=ONLI'.

            opt-dismode  = 'E'.
            opt-updmode  = 'A'.
            call transaction 'SE16' using gt_bdc options from opt.
        endcase.

      when 'STATUS'.
        case wl_saida-status.

*----------------------------------------------------------------*
*         LIMPA DOCUMENTOS                                       *
*----------------------------------------------------------------*
          when icon_red_light.
            call function 'POPUP_TO_CONFIRM'
              exporting
                titlebar              = 'Reiniciar Lançamento'
                text_question         = text-i01
                text_button_1         = 'Sim'
                icon_button_1         = 'ICON_OKAY'
                text_button_2         = 'Não'
                icon_button_2         = 'ICON_CANCEL'
                display_cancel_button = ''
              importing
                answer                = v_ans.

            case v_ans.
              when '1'.

*               Deleta os registros da Zib_erros.
                delete from zib_contabil_err where obj_key = wl_saida-objkey.
                commit work.

*               Deleta os registros da Zib_lançamentos.
                delete from zib_contabil where obj_key = wl_saida-objkey.
                commit work.

                loop at gt_saida into wl_saida where belnr = wl_saida-belnr.
                  v_index = sy-tabix.

                  clear: wl_saida-estilo, gt_estilo[].

                  wl_saida-status = icon_light_out.
                  wl_saida-objkey = space.

                  r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                                 style     = cl_gui_alv_grid=>mc_style_enabled ).

                  r_utils->z_style_disable_edit( fieldname = 'TX_AJUSTE'
                                                 style     = cl_gui_alv_grid=>mc_style_enabled ).

                  r_utils->z_style_disable_edit( fieldname = 'VLR_AJUS'
                                                 style     = cl_gui_alv_grid=>mc_style_enabled ).

                  insert lines of gt_estilo into table wl_saida-estilo.
                  modify gt_saida from wl_saida index v_index.

                endloop.

                call method obj_alv_0100->refresh_table_display
                  exporting
                    is_stable = wl_stable.
            endcase.

*----------------------------------------------------------------*
*         ESTORNA DOCUMENTO                                      *
*----------------------------------------------------------------*
          when icon_green_light.
            call function 'POPUP_TO_CONFIRM'
              exporting
                titlebar              = 'Estornar Documento'
                text_question         = text-i02
                text_button_1         = 'Sim'
                icon_button_1         = 'ICON_OKAY'
                text_button_2         = 'Não'
                icon_button_2         = 'ICON_CANCEL'
                display_cancel_button = ''
              importing
                answer                = v_ans.

            case v_ans.
              when '1'.

                select single *
                  from zib_contabil_chv
                  into wl_zib_contabil_chv
                 where obj_key = wl_saida-objkey.

                perform f_preencher_dynpro using:
                'X' 'SAPMF05A'                '0105',
                ' ' 'RF05A-BELNS'             wl_zib_contabil_chv-belnr,
                ' ' 'BKPF-BUKRS'              wl_zib_contabil_chv-bukrs,
                ' ' 'RF05A-GJAHS'             wl_zib_contabil_chv-gjahr,
                ' ' 'UF05A-STGRD'             '01',
                ' ' 'BDC_OKCODE'             '=BU'.

                call transaction 'FB08' using gt_bdc mode 'N'.

                if ( sy-subrc is initial ).

                  delete from zib_contabil_chv where obj_key = wl_saida-objkey.
                  commit work.

                  update zib_contabil set rg_atualizado = 'E' where obj_key = wl_saida-objkey.
                  commit work.

                  loop at  gt_saida into wl_saida where belnr = wl_saida-belnr.
                    v_index = sy-tabix.

                    clear: wl_saida-estilo, gt_estilo[].

                    wl_saida-status = icon_storno.

                    r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                                   style     = cl_gui_alv_grid=>mc_style_enabled ).

                    r_utils->z_style_disable_edit( fieldname = 'TX_AJUSTE'
                                                   style     = cl_gui_alv_grid=>mc_style_enabled ).

                    r_utils->z_style_disable_edit( fieldname = 'VLR_AJUS'
                                                   style     = cl_gui_alv_grid=>mc_style_enabled ).

                    insert lines of gt_estilo into table wl_saida-estilo.
                    modify gt_saida from wl_saida index v_index.
                  endloop.

                  call method obj_alv_0100->refresh_table_display
                    exporting
                      is_stable = wl_stable.

                  message s836(sd) with text-s02 display like 'S'.
                else.

                  message s836(sd) with text-e03 display like 'E'.
                endif.
            endcase.

          when icon_checked.

            delete from zib_contabil where obj_key = wl_saida-objkey.
            commit work.

            loop at  gt_saida into wl_saida where belnr = wl_saida-belnr.
              v_index = sy-tabix.

              clear: wl_saida-estilo, gt_estilo[].

              wl_saida-status = icon_light_out.
              wl_saida-objkey = space.

              r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                             style     = cl_gui_alv_grid=>mc_style_enabled ).

              r_utils->z_style_disable_edit( fieldname = 'TX_AJUSTE'
                                             style     = cl_gui_alv_grid=>mc_style_enabled ).

              r_utils->z_style_disable_edit( fieldname = 'VLR_AJUS'
                                             style     = cl_gui_alv_grid=>mc_style_enabled ).

              insert lines of gt_estilo into table wl_saida-estilo.
              modify gt_saida from wl_saida index v_index.
            endloop.

            call method obj_alv_0100->refresh_table_display
              exporting
                is_stable = wl_stable.
        endcase.
    endcase.
  endmethod.                    "ON_CLICK
endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_toolbar definition.
  public section.
    methods: constructor
                       importing io_alv_grid type ref to cl_gui_alv_grid.

    class-methods:
    set_toolbar  for event toolbar of cl_gui_alv_grid
                       importing e_object.

    class-methods:
    get_ucomm    for event user_command of cl_gui_alv_grid
                       importing e_ucomm.
endclass.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_toolbar implementation.
  method constructor.
    clear: obj_toolbar_manager.
    create object obj_toolbar_manager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.                    "CONSTRUCTOR

  method set_toolbar.

    wl_toolbar-function     = 'BTN_GERAR'.
    wl_toolbar-icon         =  icon_generate.
    wl_toolbar-butn_type    = 0.
    wl_toolbar-text         = 'Gerar Lançamento Contábil →'.

    append wl_toolbar to e_object->mt_toolbar.
    clear wl_toolbar.

    wl_toolbar-butn_type    = 3.
    append wl_toolbar to e_object->mt_toolbar.
    clear wl_toolbar.

    wl_toolbar-function     = 'BTN_FINALIZAR'.
    wl_toolbar-icon         = icon_okay.
    wl_toolbar-butn_type    = 0.
    wl_toolbar-text         = 'Finalizar Lançamento'.

    append wl_toolbar to e_object->mt_toolbar.
    clear wl_toolbar.

    wl_toolbar-butn_type    = 3.
    append wl_toolbar to e_object->mt_toolbar.
    clear wl_toolbar.

    wl_toolbar-function     = 'BTN_MARCAR_TODOS'.
    wl_toolbar-icon         = icon_system_mark.
    wl_toolbar-butn_type    = 0.
    wl_toolbar-text         = 'Selecionar todos'.

    append wl_toolbar to e_object->mt_toolbar.
    clear wl_toolbar.

    wl_toolbar-butn_type    = 3.
    append wl_toolbar to e_object->mt_toolbar.
    clear wl_toolbar.

    wl_toolbar-function     = 'BTN_REFRESH'.
    wl_toolbar-icon         = icon_refresh.
    wl_toolbar-butn_type    = 0.
    wl_toolbar-text         = 'Atualizar'.

    append wl_toolbar to e_object->mt_toolbar.
    clear wl_toolbar.

    call method obj_toolbar_manager->reorganize
      exporting
        io_alv_toolbar = e_object.
  endmethod.                    "SET_TOOLBAR

  method get_ucomm.
    data: r_utils      type ref to lcl_utils,
          at_row_id	   type lvc_s_row,
          at_column_id type lvc_s_col.

    create object r_utils.
    clear gt_zib_contabil.

    case e_ucomm.
      when 'BTN_GERAR'.

        data: at_data_ref       type char20,
              at_index          type sy-tabix,
              at_bldat          type char10,
              at_budat          type char10,
              at_zfbdt          type char10,
              at_belnr          type bkpf-belnr,
              at_maior_negativo type dmbtr,
              at_maior_positivo type dmbtr,
              at_diferenca      type dmbtr,
              vlr_0             type dmbtr,
              at_cont           type c value 1.

        loop at gt_saida into wl_saida where check = 'X'.
          clear: gt_estilo[], wl_saida-estilo.

          at_index = sy-tabix.

          if ( wl_saida-objkey is not initial ).
            at_cont = 1.

            while ( at_cont <= 3 ).
              clear wl_zib_contabil.
              r_utils->concatenar_objkey( exporting
                                          i_belnr  = wl_saida-belnr
                                          i_gjahr  = wl_saida-gjahr
                                          i_estor  = c_x
                                          i_seq    = at_cont
                                          importing
                                          e_objkey = at_obj_key ).

              select single *
                from zib_contabil
                into wl_zib_contabil
               where obj_key = at_obj_key.

              if ( not sy-subrc is initial ).
                exit.
              else.
                at_cont = at_cont + 1.
              endif.
            endwhile.

          else.
            r_utils->concatenar_objkey( exporting
                                        i_belnr  = wl_saida-belnr
                                        i_gjahr  = wl_saida-gjahr
                                        i_estor  = space
                                        i_seq    = space
                                        importing
                                        e_objkey = at_obj_key ).
          endif.

          concatenate wl_saida-budat+4(2) wl_saida-gjahr
                      into at_data_ref separated by '/'.

          if ( wl_saida-umskz is initial ).

            case wl_saida-koart.
              when 'D'.
                if wl_saida-vlr_ajus > vlr_0.
                  wl_zib_contabil-bschl  = '01'.
                else.
                  wl_zib_contabil-bschl  = '11'.
                endif.

              when 'K'.
                if wl_saida-vlr_ajus > vlr_0.
                  wl_zib_contabil-bschl  = '21'.
                else.
                  wl_zib_contabil-bschl  = '31'.
                endif.

              when 'S'.
                if wl_saida-vlr_ajus > vlr_0.
                  wl_zib_contabil-bschl = '40'.
                else.
                  wl_zib_contabil-bschl = '50'.
                endif.
            endcase.

          else.

            case wl_saida-koart.
              when 'D'.
                if wl_saida-vlr_ajus > vlr_0.
                  wl_zib_contabil-bschl  = '09'.
                else.
                  wl_zib_contabil-bschl  = '19'.
                endif.

              when 'K'.
                if wl_saida-vlr_ajus > vlr_0.
                  wl_zib_contabil-bschl  = '29'.
                else.
                  wl_zib_contabil-bschl  = '39'.
                endif.
            endcase.

          endif.

          case wl_saida-koart.
            when 'D'.
              wl_zib_contabil-hkont = wl_saida-kunnr. "Cliente
            when 'K'.
              wl_zib_contabil-hkont = wl_saida-lifnr. "Fornecedor
            when others.
              wl_zib_contabil-hkont = wl_saida-hkont. "Outros
          endcase.

*        ----- Calcula diferença do Valor do Ajuste -----

          if ( at_belnr <> wl_saida-belnr ).

            clear: at_maior_positivo,
                   at_maior_negativo,
                   at_diferenca.

            at_belnr = wl_saida-belnr.

            r_utils->calcular_diferenca( exporting
                                         i_belnr          = at_belnr
                                         importing
                                         e_maior_negativo = at_maior_negativo
                                         e_maior_positivo = at_maior_positivo
                                         e_diferenca      = at_diferenca ).
          endif.

          if ( at_diferenca < vlr_0 ).
            if wl_saida-vlr_ajus = at_maior_negativo.
              wl_saida-vlr_ajus = wl_saida-vlr_ajus - at_diferenca.
              clear at_maior_negativo.
            endif.

          elseif ( at_diferenca > vlr_0 ).
            if wl_saida-vlr_ajus = at_maior_positivo.
              wl_saida-vlr_ajus = wl_saida-vlr_ajus - at_diferenca.
              clear at_maior_positivo.
            endif.
          endif.

          if ( wl_saida-vlr_ajus < vlr_0 ).
            wl_zib_contabil-dmbe2 = wl_saida-vlr_ajus * -1.
          else.
            wl_zib_contabil-dmbe2 = wl_saida-vlr_ajus.
          endif.

*         ----- fim ----

          call function 'CONVERSION_EXIT_PDATE_OUTPUT'
            exporting
              input  = wl_saida-bldat
            importing
              output = at_bldat.

          wl_zib_contabil-budat = wl_saida-budat.

          if ( wl_zib_contabil-budat+0(6) = 201509 ).
            wl_zib_contabil-budat = '20151001'.
          endif.

          call function 'CONVERSION_EXIT_PDATE_OUTPUT'
            exporting
              input  = wl_zib_contabil-budat
            importing
              output = wl_zib_contabil-budat.

          call function 'CONVERSION_EXIT_PDATE_OUTPUT'
            exporting
              input  = wl_saida-zfbdt
            importing
              output = at_zfbdt.

          if ( wl_saida-status = icon_checked ).
            wl_zib_contabil-rg_atualizado = c_f.
          else.
            wl_saida-status               = icon_yellow_light.
            wl_zib_contabil-rg_atualizado = c_n.
          endif.

          move:
          space               to wl_saida-check,
          at_obj_key          to wl_saida-objkey,
*          at_budat            to wl_zib_contabil-budat,
          at_bldat            to wl_zib_contabil-bldat,
          at_zfbdt            to wl_zib_contabil-zfbdt,
          at_obj_key          to wl_zib_contabil-obj_key,
          at_index            to wl_zib_contabil-seqitem,
          wl_saida-gsber      to wl_zib_contabil-gsber,
          wl_saida-bukrs      to wl_zib_contabil-bukrs,
          wl_saida-gjahr      to wl_zib_contabil-gjahr,
          wl_zib_contabil-budat+3(2) to wl_zib_contabil-monat,
          c_tipo_doc          to wl_zib_contabil-blart,
          wl_saida-waers      to wl_zib_contabil-waers,
          wl_saida-waers      to wl_zib_contabil-waers,
          wl_saida-zlspr      to wl_zib_contabil-zlspr,
          wl_saida-zlsch      to wl_zib_contabil-zlsch,
          wl_saida-aufnr      to wl_zib_contabil-aufnr,
          wl_saida-kostl      to wl_zib_contabil-kostl,
          c_x                 to wl_zib_contabil-waers_i,
          c_usd               to wl_zib_contabil-waers_f,
          wl_saida-tx_ajuste  to wl_zib_contabil-xref3,
          at_data_ref         to wl_zib_contabil-bktxt,
          wl_saida-sgtxt      to wl_zib_contabil-sgtxt,
          wl_saida-matnr      to wl_zib_contabil-matnr,
          wl_saida-prctr      to wl_zib_contabil-prctr,
          wl_saida-xblnr      to wl_zib_contabil-xblnr,
          wl_saida-land1      to wl_zib_contabil-land1,
          wl_saida-kidno      to wl_zib_contabil-kidno,
          wl_saida-blart      to wl_zib_contabil-blart,
          wl_saida-zuonr      to wl_zib_contabil-zuonr.

          append wl_zib_contabil to gt_zib_contabil.
          clear wl_zib_contabil.

          r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).

          r_utils->z_style_disable_edit( fieldname = 'TX_AJUSTE'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).

          r_utils->z_style_disable_edit( fieldname = 'VLR_AJUS'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).

          insert lines of gt_estilo into table wl_saida-estilo.
          modify gt_saida from wl_saida index at_index.
        endloop.

        if ( sy-subrc is initial ).

          modify zib_contabil  from table gt_zib_contabil.
          commit work.

          call method obj_alv_0100->refresh_table_display
            exporting
              is_stable = wl_stable_aux.

        else.
          message text-e02 type 'I' display like 'E'.
        endif.

      when 'BTN_FINALIZAR'.
        loop at gt_saida into wl_saida where check  = 'X'
                                         and status = icon_light_out.
*                                          OR STATUS = ICON_CHECKED.

*          AT_ROW_ID    = SY-TABIX.
*          AT_COLUMN_ID = 'STATUS'.
*
*          IF ( WL_SAIDA-STATUS = ICON_CHECKED ).
*
*            WL_SAIDA-STATUS    = ICON_RED_LIGHT.
*
*            MODIFY GT_SAIDA FROM WL_SAIDA INDEX SY-TABIX
*            TRANSPORTING STATUS.
*
*            LCL_EVENT_HANDLER=>ON_CLICK( E_ROW_ID    = AT_ROW_ID
*                                         E_COLUMN_ID = AT_COLUMN_ID ).
*          ELSE.
          wl_saida-status = icon_checked.

          modify gt_saida from wl_saida index sy-tabix
          transporting status.

        endloop.

        lcl_event_toolbar=>get_ucomm( e_ucomm = 'BTN_GERAR' ).
*          ENDIF.

*          MODIFY GT_SAIDA FROM WL_SAIDA INDEX SY-TABIX
*          TRANSPORTING STATUS.
*        ENDLOOP.

        call method obj_alv_0100->refresh_table_display
          exporting
            is_stable = wl_stable_aux.

      when 'BTN_MARCAR_TODOS'.
        loop at gt_saida into wl_saida where status = icon_light_out.
          wl_saida-check = c_x.

          modify gt_saida from wl_saida index sy-tabix
          transporting check.
        endloop.

        call method obj_alv_0100->refresh_table_display
          exporting
            is_stable = wl_stable_aux.

      when 'BTN_REFRESH'.
        loop at gt_saida into wl_saida where status = icon_yellow_light.

*         Verifica se o lançamento foi gerado com sucesso.
          select single *
            from zib_contabil_chv
            into wl_zib_contabil_chv
           where obj_key = wl_saida-objkey.

          if ( sy-subrc is initial ).
            wl_saida-status = icon_green_light.
          else.

*         Senão, verifica se houve falha no lançamento.
            select single *
              from zib_contabil_err
              into wl_zib_contabil_err
             where obj_key = wl_saida-objkey.

            if ( sy-subrc is initial ).
              wl_saida-status = icon_red_light.
            endif.
          endif.

          check ( sy-subrc is initial ).
          modify gt_saida from wl_saida index sy-tabix
          transporting status.
        endloop.

        if ( sy-subrc is initial ).
          call method obj_alv_0100->refresh_table_display
            exporting
              is_stable = wl_stable_aux.
        endif.

        message s836(sd) with text-s01 display like 'S'.
    endcase.
  endmethod.                    "GET_UCOMM
endclass.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION
