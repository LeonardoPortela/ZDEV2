*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_6001
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_legenda,
         icone TYPE char6,
         entid TYPE char20,
         descr TYPE char50.
TYPES: END OF ty_legenda.

DATA: g_custom_container_pop_6001 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_pop_6001           TYPE REF TO cl_gui_alv_grid,
      gs_layout_pop_6001          TYPE lvc_s_layo,
      it_fieldcatalog_pop_6001    TYPE lvc_t_fcat.

DATA: it_legenda TYPE STANDARD TABLE OF ty_legenda.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6001_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_6001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_6001  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_6001 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T6001'.

  PERFORM monta_pop_6001.
  PERFORM mostra_pop_6001.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6001  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_6001 INPUT.

  CASE sy-ucomm.
    WHEN 'SALVAR'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_6001
*&---------------------------------------------------------------------*
FORM mostra_pop_6001.

  IF g_custom_container_pop_6001 IS INITIAL.

    CREATE OBJECT g_custom_container_pop_6001
      EXPORTING
        container_name              = 'CONTAINER6001'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_6001 USING:
          01 'ICONE'          ''         ' '  ' ' 'X'  ' '   'X'   ' '   ' '   ' '   'Ícone',
          01 'ENTID'          ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Entidade',
          01 'DESCR'          ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Descrição'.

    gs_layout_pop_6001-cwidth_opt = 'X'.

    CREATE OBJECT ctl_alv1_pop_6001
      EXPORTING
        i_parent = g_custom_container_pop_6001.           "ALV Lote

    CALL METHOD ctl_alv1_pop_6001->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_pop_6001
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_pop_6001
        it_outtab       = it_legenda.

  ELSE.
    CALL METHOD ctl_alv1_pop_6001->refresh_table_display
      EXPORTING
        is_stable = _stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_6001
*&---------------------------------------------------------------------*
FORM monta_pop_6001.

  "pelo numero da tela colocar ícones e legendas

  DATA: wa_legenda TYPE ty_legenda.

  CLEAR: it_legenda.

  IF vg_subt_lote EQ '5120' OR
     vg_subt_lote EQ '5130' OR
     vg_subt_lote EQ '5140' OR
     vg_subt_lote EQ '5220' OR
     vg_subt_lote EQ '5230' OR
     vg_subt_lote EQ '5320'.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5C@'.
    wa_legenda-entid = 'Lote'.
    wa_legenda-descr = 'Lote não Aprovado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5B@'.
    wa_legenda-entid = 'Lote'.
    wa_legenda-descr = 'Lote Aprovado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@7E@'.
    wa_legenda-entid = 'Lote'.
    wa_legenda-descr = 'Lote em Carga'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@9K@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Carga Criada'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5D@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Carga sem Cotação'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@FD@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Carga Cotada'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@4A@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Frete Contratado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@96@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Embarque Autorizado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@0Q@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Romaneio Gerado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@01@'.
    wa_legenda-entid = 'Romaneio'.
    wa_legenda-descr = 'Romaneio Finalizado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5Y@'.
    wa_legenda-entid = 'Nota Fiscal'.
    wa_legenda-descr = 'Nota Fiscal Autorizado'.
    APPEND wa_legenda TO it_legenda.

  ELSEIF vg_subt_lote EQ '5420' OR
       vg_subt_lote EQ '5520' OR
       vg_subt_lote EQ '5620'.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5B@'.
    wa_legenda-entid = 'Caminhão'.
    wa_legenda-descr = 'Caminhão sem Romaneio'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@0Q@'.
    wa_legenda-entid = 'Caminhão'.
    wa_legenda-descr = 'Romaneio Gerado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@01@'.
    wa_legenda-entid = 'Caminhão'.
    wa_legenda-descr = 'Romaneio Finalizado'.
    APPEND wa_legenda TO it_legenda.

    "US #154513 - MMSILVA - 28.02.2025 - Inicio
    CLEAR wa_legenda.
    wa_legenda-icone = '@5Y@'.
    wa_legenda-entid = 'Nota Fiscal'.
    wa_legenda-descr = 'Nota Fiscal Emitida'.
    APPEND wa_legenda TO it_legenda.
    "US #154513 - MMSILVA - 28.02.2025 - Fim

  ELSEIF vg_subt_lote EQ '5720' OR
       vg_subt_lote EQ '5820'.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5C@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Carga não Aprovada'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5B@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Carga Aprovada'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@0Q@'.
    wa_legenda-entid = 'Carga'.
    wa_legenda-descr = 'Romaneio Gerado'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@01@'.
    wa_legenda-entid = 'Romaneio'.
    wa_legenda-descr = 'Romaneio Finalizado'.
    APPEND wa_legenda TO it_legenda.

*-CS2021000218-31.08.2022-#90368-JT-inicio
    CLEAR wa_legenda.
    wa_legenda-icone = '@00@'.
    wa_legenda-entid = 'Solic.Receituário'.
    wa_legenda-descr = 'Sem necessidade de Solic.Receituário'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5C@'.
    wa_legenda-entid = 'Solic.Receituário'.
    wa_legenda-descr = 'Solic. Receituário Pendente de Geração'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5B@'.
    wa_legenda-entid = 'Solic.Receituário'.
    wa_legenda-descr = 'Solic. Receituário Gerada/Finalizada'.
    APPEND wa_legenda TO it_legenda.
*----------------------------------------------
    CLEAR wa_legenda.
    wa_legenda-icone = '@00@'.
    wa_legenda-entid = 'Receituário'.
    wa_legenda-descr = 'Sem necessidade de Receituário'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5C@'.
    wa_legenda-entid = 'Receituário'.
    wa_legenda-descr = 'Receituário Pendente de Geração'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5D@'.
    wa_legenda-entid = 'Receituário'.
    wa_legenda-descr = 'Receituário Pendente de Finalização'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5B@'.
    wa_legenda-entid = 'Receituário'.
    wa_legenda-descr = 'Receituário Gerado'.
    APPEND wa_legenda TO it_legenda.
*-CS2021000218-31.08.2022-#90368-JT-inicio

    "US #154513 - MMSILVA - 28.02.2025 - Inicio
    CLEAR wa_legenda.
    wa_legenda-icone = '@5Y@'.
    wa_legenda-entid = 'Nota Fiscal'.
    wa_legenda-descr = 'Nota Fiscal Emitida'.
    APPEND wa_legenda TO it_legenda.
    "US #154513 - MMSILVA - 28.02.2025 - Fim
  ENDIF.

ENDFORM.
