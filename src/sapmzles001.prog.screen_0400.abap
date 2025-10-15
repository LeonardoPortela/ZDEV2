
process before output.

* MODULE STATUS_400.
  module inicializa_detalhes_400.

  module visibilidade_valores_400.

  loop at ti_400_confer with control vg_sbs400_tabcontrol.
    module change_400_confer.
  endloop.

  call subscreen: sub401 including sy-repid '0401',
                  sub404 including sy-repid '0404'.

process after input.

  loop at ti_400_confer.

    module read_table_control_400.

    field ti_400_confer-conhec
          module chama_vt03_tknum_400 at cursor-selection.

    field ti_400_confer-check
          module altera_item_marcado_400 on request.

  endloop.

  field sdetlh_confer_400-docsap1
        module chama_fb03_docsap_400 at cursor-selection.

  field sdetlh_confer_400-docsap2
        module chama_fb03_docsap_400 at cursor-selection.

  field sdetlh_confer_400-docsap3
        module chama_fb03_docsap_400 at cursor-selection.

  field sdetlh_confer_400-docsap4
        module chama_fb03_docsap_400 at cursor-selection.

  module check_okcode_400.

  call subscreen: sub401,
                  sub404.
