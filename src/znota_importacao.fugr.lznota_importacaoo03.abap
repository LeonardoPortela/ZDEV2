*----------------------------------------------------------------------*
***INCLUDE LZNOTA_IMPORTACAOO03 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_ADICOES_DI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZNOTA_IMPORT  text
*----------------------------------------------------------------------*
form seleciona_adicoes_di  using p_import type znota_import.

  select * into table it_znota_import_ad
    from znota_import_ad
   where docnum  eq p_import-docnum
     and itmnum  eq p_import-itmnum
     and itdidoc eq p_import-itdidoc.

  vg_itdidoc = p_import-itdidoc.

endform.                    " SELECIONA_ADICOES_DI

*&---------------------------------------------------------------------*
*&      Module  STATUS_2003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_2003 output.

  perform cria_alv_adicao_di.

endmodule.                 " STATUS_2003  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_ADICAO_DI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form cria_alv_adicao_di .

  constants: tabela_di type string value 'IT_ZNOTA_IMPORT_AD'.

  data: text_n000 type c length 50 value 'Nr. Adição',
        text_n001 type c length 50 value 'Nr. Sequência do item dentro da adição',
        text_n002 type c length 50 value 'Código do fabricante estrangeiro',
        text_n003 type c length 50 value 'Valor do desconto do item da DI – Adição',
        text_n004 type c length 50 value 'Número do Pedido de Compra',
        text_n005 type c length 50 value 'Item do Pedido de Compra'.

  if imp_prim_di_ad is initial.
*
**   Create object for container
    create object imp_container_di_ad
      exporting
        container_name = 'CUSTOMCONTROL_AD'.
*
    create object imp_alv_di_ad
      exporting
        i_parent = imp_container_di_ad.

    perform z_estrutura_fieldcat tables it_imp_catalog_di_ad using:
        tabela_di 'NR_ADICAO'        text_n000 'X'   01 04 space space space space space space,
        tabela_di 'NR_SEQ_ADICAO'    text_n001 space 02 04 space space space space space space,
        tabela_di 'CFABRICANTE'      text_n002 space 03 20 space space space space space space,
        tabela_di 'VLR_DESCONTO'     text_n003 space 04 15 space space space space space space,
        tabela_di 'NR_PED_COMPRA'    text_n004 space 05 15 space space space space space space,
        tabela_di 'NR_PED_COMPRA_IT' text_n005 space 06 15 space space space space space space.

    imp_gs_layout-zebra    = c_x.
    imp_gs_layout-sel_mode = space.

    create object imp_event_di_ad.
    set handler imp_event_di_ad->handle_hotspot_di_ad for imp_alv_di_ad.

    call method imp_alv_di_ad->set_table_for_first_display
      exporting
        i_default       = space
        is_layout       = imp_gs_layout
      changing
        it_fieldcatalog = it_imp_catalog_di_ad
        it_outtab       = it_znota_import_ad[].

    imp_prim_di_ad = c_x.

  endif.

  call method imp_alv_di_ad->refresh_table_display.

endform.                    " CRIA_ALV_ADICAO_DI
