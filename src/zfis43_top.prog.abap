*&---------------------------------------------------------------------*
*& Include          ZFIS43_TOP
*&---------------------------------------------------------------------*

tables: adrc,
        j_1bbranch.

types:
  begin of ty_branch,
    bukrs  type j_1bbranch-bukrs,
    branch type j_1bbranch-branch,
  end of ty_branch.

types:
  begin of ty_filial_conf.
    include structure zfis_filial_conf.
types celltab type lvc_t_styl.
types mark(1).
types end of ty_filial_conf.

types:
  begin of ty_filial_desc.
    include structure zfis_filial_desc.
types celltab type lvc_t_styl.
types mark(1).
types end of ty_filial_desc.

data: t_branch      type table of ty_branch,
      t_filial_conf type table of zfis_filial_conf,
      t_filial_desc type table of zfis_filial_desc,
      t_saida_conf  type table of ty_filial_conf,
      w_saida_conf  type ty_filial_conf,
      t_saida_desc  type table of ty_filial_desc,
      w_saida_desc  type ty_filial_desc.

data: it_rows   type lvc_t_row,
      wa_row    type lvc_s_row,
      wa_stable type lvc_s_stbl.

class:  lcl_alv_toolbar     definition deferred.

data: g_container          type scrfname value 'BCALV_GRID_DEMO_0100_CONT1',
      g_grid               type ref to cl_gui_alv_grid,
      g_custom_container   type ref to cl_gui_custom_container,
      gs_layout            type lvc_s_layo,
      obg_toolbar          type ref to lcl_alv_toolbar,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager.

*Declaration for toolbar buttons
data: ty_toolbar type stb_button.


class lcl_alv_toolbar definition.
  public section.
*Constructor
    methods:
      constructor
        importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command2 for event user_command of cl_gui_alv_grid
        importing e_ucomm,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.

class lcl_alv_toolbar implementation.
  method constructor.
*   Create ALV toolbar manager instance
    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.

  method on_toolbar.
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-disabled  = ' '.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    call method c_alv_toolbarmanager->reorganize
      exporting
        io_alv_toolbar = e_object.

  endmethod.

  method handle_user_command.
    case e_ucomm.
      when 'DEL'.
        call method g_grid->get_selected_rows
          importing
            et_index_rows = it_rows.

        loop at it_rows into wa_row.
          read table t_saida_conf into w_saida_conf index wa_row-index.
          w_saida_conf-mark = 'X'.
          modify  t_saida_conf from w_saida_conf index wa_row-index transporting mark.
          delete from  zfis_filial_conf where filial    = w_saida_conf-filial
                                        and   tipo_nota = w_saida_conf-tipo_nota.
        endloop.
    endcase.
    commit work.
    delete t_saida_conf where mark = 'X'.
    call method g_grid->refresh_table_display
      exporting
        is_stable = wa_stable.

    call method cl_gui_cfw=>flush.
  endmethod.

  method handle_user_command2.
    case e_ucomm.
      when 'DEL'.
        call method g_grid->get_selected_rows
          importing
            et_index_rows = it_rows.

        loop at it_rows into wa_row.
          read table t_saida_desc into w_saida_desc index wa_row-index.
          w_saida_desc-mark = 'X'.
          modify  t_saida_desc from w_saida_desc index wa_row-index transporting mark.
          delete from  zfis_filial_desc where branch    = w_saida_desc-branch.
        endloop.
    endcase.
    commit work.
    delete t_saida_desc where mark = 'X'.
    call method g_grid->refresh_table_display
      exporting
        is_stable = wa_stable.

    call method cl_gui_cfw=>flush.
  endmethod.


endclass.
