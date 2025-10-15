*&---------------------------------------------------------------------*
*& Report  ZMMR129
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zmmr129.
tables: mard,  marc, mara, mcha, lfa1, zppt0011.
type-pools: slis.
types:  kkblo_selfield type slis_selfield.

*----------------------------------------------------------------------*
*       INTERFACE ZIF_DATA_READER
*----------------------------------------------------------------------*
interface: zif_data_reader.
  types: begin of ty_saida,
           matnr       type mard-matnr,       "Material
           maktx       type makt-maktx,       "Desc. Material
           mtart       type mara-mtart,       "Tipo material
           meins       type mara-meins,       " Unidade de medida
           werks       type mard-werks,       "Centro
           lgort       type mard-lgort,       "Depósito
           charg       type mchb-charg,       "Lote
           "vfdat       TYPE mch1-vfdat,      "vencimento
           diasr       type i,                "dias restantes
           clabs       type mchb-clabs,       "Quantidade em estoque
           lifnr       type mslb-lifnr,       "Código do fornecedor
           lblab       type mslb-lblab,       "Estoque de utilização livre avaliado
           matkl       type mara-matkl,       "Código do grupo do material
           hsdat       type mcha-hsdat,       "Data de produção
           licha       type mcha-licha,       "Nº do lote fornecedor
           umlmc       type marc-umlmc,       "Estoque em transferência (centro a centro)
           trame       type marc-trame,       "Estoque em trânsito
           status(20),                        "Status
           saldo_z(02),                       "Saldo
           name1       type lfa1-name1,       "Nome do fornecedor                              "User Story 144142 // MMSILVA 02.10.2024
           vfdat       type zppt0011-vfdat,   "Data do vencimento                              User Story 144142 // MMSILVA 02.10.2024
           pvprs       type ckmlcr-pvprs,     "Preço interno periódico                         User Story 144142 // MMSILVA 04.10.2024
           salk3       type ckmlcr-salk3,     "Valor do estoque avaliado total                 User Story 144142 // MMSILVA 04.10.2024
           lifnr_ped   type ekko-lifnr,       "Nº conta do fornecedor pedido compra            User Story 144142 // MMSILVA 23.10.2024
           name1_ped   type lfa1-name1,       "Nome fornecedor pedido compra                   User Story 144142 // MMSILVA 23.10.2024
         end of ty_saida,

         begin of ty_popup,
           werks type t001w-werks,
           vkorg type t001w-vkorg,
           name1 type t001w-name1,
         end of ty_popup.

  methods: read_data, generate_grid.
endinterface.                    "ZIF_DATA_READER

data: it_saida     type table of zif_data_reader=>ty_saida,
      it_saida_aux type table of zif_data_reader=>ty_saida,
      vg_erro      type char1,
      r_werks      type range of werks_d,
      wa_saida     type zif_data_reader=>ty_saida.

data: go_container type ref to cl_gui_custom_container,
      go_alv_popup type ref to cl_gui_alv_grid,
      gt_popup     type table of zif_data_reader=>ty_popup,
      gv_continue  type c,
      gt_fieldcat  type lvc_t_fcat,
      gs_fcat      like line of gt_fieldcat.

selection-screen: begin of block b1 with frame title text-001.
  selection-screen skip 1.
  select-options: s_cent  for mard-werks obligatory,
                  s_matn  for mard-matnr,
                  s_lgor  for mard-lgort,
                  s_matkl for mara-matkl.

  parameters: p_group as checkbox.  " Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo

selection-screen: end of block b1.


initialization.

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS DEFINITION
*----------------------------------------------------------------------*
class lcl_seleciona_dados definition.

  public section.
    interfaces zif_data_reader.

  private section.

*VARIÁVEIS
    data:
          v_text   type char45.

*TABELAS INTERNAS*
    data: it_mchb     type table of mchb,
          it_mch1     type table of mch1,
          it_zppt0011 type table of zppt0011, " Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
          it_zppt0016 type table of zppt0016, " Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
          it_mslb     type table of mslb,
          it_mara     type table of mara,
          it_mara_aux type table of mara,
          it_mcha     type table of mcha,
          it_marc     type table of marc,
          it_marc_aux type table of marc,

          it_makt     type table of makt,
          it_fcat     type table of slis_fieldcat_alv,
          it_evento   type          slis_t_event,
          it_lfa1     type table of lfa1, "User Story 144142 // MMSILVA 02.10.2024
          it_ckmlhd   type table of ckmlhd, "User Story 144142 // MMSILVA 04.10.2024
          it_ckmlcr   type table of ckmlcr, "User Story 144142 // MMSILVA 04.10.2024
          it_ekko     type table of ekko, "User Story 144142 // MMSILVA 23.10.2024
          it_mard     type table of mard. "User Story 144142 // MMSILVA 23.10.2024

    data: r_matnr  type range of matnr,
          vg_matnr type matnr,
          vg_werks type werks_d.


*TABELAS WORK-ÁREAS*
    data: wa_mchb     type mchb,
          wa_marc     type marc,
          wa_mch1     type mch1,
          wa_mslb     type mslb,
          wa_mara     type mara,
          wa_makt     type makt,
          wa_fcat     type slis_fieldcat_alv,
          wa_evento   type slis_alv_event,
          wa_layout   type slis_layout_alv,
          wa_saida    type zif_data_reader=>ty_saida,
          wa_lfa1     type lfa1, "User Story 144142 // MMSILVA 02.10.2024
          wa_zppt0011 type zppt0011, "User Story 144142 // MMSILVA 02.10.2024
          wa_ckmlhd   type ckmlhd, "User Story 144142 // MMSILVA 04.10.2024
          wa_ckmlcr   type ckmlcr, "User Story 144142 // MMSILVA 04.10.2024
          wa_ekko     type ekko, "User Story 144142 // MMSILVA 23.10.2024
          wa_mard     type mard. "User Story 144142 // MMSILVA 23.10.2024


endclass.                    "LCL_SELECIONA_DADOS DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_MONTA_FIELDCAT DEFINITION
*----------------------------------------------------------------------*
class lcl_monta_fieldcat definition.

  public section.
    methods: set_fieldcat importing i_campo  type char30
                                    i_desc_l type char40
                                    i_tam    type num6.


    methods: get_fieldcat exporting e_campo  type char30
                                    e_desc_l type char40
                                    e_tam    type num6.

    data: at_campo  type char30,
          at_desc_l type char40,
          at_tam    type num6.
endclass.                    "LCL_MONTA_FIELDCAT DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_seleciona_dados implementation.
  method zif_data_reader~read_data.

    clear: vg_erro.

    data: vfor(1).
    "Selecionar dados
    if s_matkl is not initial.
      select *
      from mara
      into table it_mara
      where matnr in s_matn
        and matkl in s_matkl.

      if it_mara is not initial.
        select *
          from mchb
          into table it_mchb
          for all entries in it_mara
          where matnr eq it_mara-matnr
          and   werks in r_werks
          and   lgort in s_lgor
          and   ( clabs ne 0 or cinsm ne 0  or cspem ne 0 ).
      endif.
    else.

      select *
      from mara
      into table it_mara
      where matnr in s_matn.
*        and matkl in s_matkl.
      if it_mara is not initial.
        select *
          from mchb as a
          into table it_mchb
          for all entries in it_mara
          where matnr eq it_mara-matnr
          and   werks in r_werks
          and   lgort in s_lgor
          and   ( clabs ne 0 or cinsm ne 0  or cspem ne 0 ).
      endif.
    endif.

    if it_mchb[] is not initial.
      select *
        from mch1
        into table it_mch1
        for all entries in it_mchb
        where matnr eq it_mchb-matnr
        and   charg eq it_mchb-charg.

      select *
        from makt
        into table it_makt
        for all entries in it_mchb
         where spras eq sy-langu
         and   matnr eq it_mchb-matnr.

      select *
       from mara
       into table it_mara
       for all entries in it_mchb
        where matnr eq it_mchb-matnr.
    endif.

*>>Begin-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
    if it_mchb is not initial.
      select *
        from zppt0011
        into table @it_zppt0011
        for all entries in @it_mchb
        where charg = @it_mchb-charg.

      select *
        from zppt0016
        into table @it_zppt0016
        for all entries in @it_mchb
        where matnr  = @it_mchb-matnr
          and werks  = @it_mchb-werks
          and lgort  = @it_mchb-lgort
          and chargd = @it_mchb-charg.
    endif.

*<<<End-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo

    if s_matkl is not initial.
      select *
      from mara
      into table it_mara
      where matnr in s_matn
        and matkl in s_matkl.

      if it_mara is not initial.
        "estoque especial
        select *
        from mslb as a
        into table it_mslb
          for all entries in it_mara
        where matnr eq it_mara-matnr
        and   werks in r_werks
        and   ( lblab ne 0 or lbins ne 0 ).
      endif.

    else.

      "estoque especial
      select *
      from mslb as a
      into table it_mslb
      where matnr in s_matn
      and   werks in r_werks
      and   ( lblab ne 0 or lbins ne 0 ).

    endif.

    if it_mslb[] is not initial.
      select *
          from mch1
          appending table it_mch1
          for all entries in it_mslb
          where matnr eq it_mslb-matnr
          and   charg eq it_mslb-charg.


      select *
      from makt
      appending table it_makt
      for all entries in it_mslb
      where spras eq sy-langu
      and   matnr eq it_mslb-matnr.

      select *
      from mara
      appending table it_mara
      for all entries in it_mslb
      where matnr eq it_mslb-matnr
      and matkl in s_matkl.

*     User Story 144142 // MMSILVA 04.10.2024 - Inicio
      select *
        from lfa1
        appending table it_lfa1
        for all entries in it_mslb
        where lifnr eq it_mslb-lifnr.
*     User Story 144142 // MMSILVA 04.10.2024 - Fim
    endif.




*    IF s_matkl IS NOT INITIAL.
*
*      SORT it_mchb BY matnr.
*      SORT it_mslb BY matnr.
*      r_matnr = VALUE #( FOR l IN it_mara ( sign = 'I' option = 'EQ' low = l-matnr )  ).
*      IF r_matnr IS NOT INITIAL.
*        IF it_mchb IS NOT INITIAL.
*          DELETE it_mchb WHERE matnr NOT IN r_matnr.
*        ENDIF.
*        IF it_mslb IS NOT INITIAL.
*          DELETE it_mslb WHERE matnr NOT IN r_matnr.
*        ENDIF.
*      ELSE.
*        vg_erro = abap_true.
*      ENDIF.
*    ENDIF.

*    CHECK vg_erro IS INITIAL.


    "preparar SAIDA
    sort: it_mchb by matnr werks  charg,
          it_mch1 by matnr charg,
          it_mslb by matnr werks  charg,
          it_mara by matnr,
          it_makt by matnr,
          it_lfa1 by werks.

    if it_mchb is not initial.
      free: it_mcha.
      select * from mcha
         into table it_mcha
         for all entries in it_mchb
        where matnr eq it_mchb-matnr
          and werks eq it_mchb-werks
          and charg eq it_mchb-charg.

      free: it_marc.
      select * from marc
         into table it_marc
         for all entries in it_mchb
        where matnr eq it_mchb-matnr
          and werks eq it_mchb-werks.

      if it_marc is not initial.
        sort it_marc by matnr werks.
        delete adjacent duplicates from it_marc comparing matnr werks.
      endif.
    endif.


    "Saldo em transferencia.
    if s_matkl is not initial.
      select *
        from mara
        appending table it_mara_aux
        where matnr in s_matn
        and matkl   in s_matkl.
      if it_mara_aux is not initial.
        free: it_marc_aux.
        select * from marc
          into table it_marc_aux
          for all entries in it_mara_aux
          where matnr eq it_mara_aux-matnr
          and werks in r_werks
          and ( umlmc ne 0 or trame ne 0 ).
        if it_marc_aux is not initial.
          select *
          from makt
          appending table it_makt
          for all entries in it_marc_aux
          where spras eq sy-langu
          and   matnr eq it_marc_aux-matnr.
        endif.
      endif.

    else.

      free: it_marc_aux.
      select * from marc
        into table it_marc_aux
        where matnr in s_matn
          and werks in r_werks
          and ( umlmc ne 0 or trame ne 0 ).
      if it_marc_aux is not initial.
        select *
        from makt
        appending table it_makt
        for all entries in it_marc_aux
        where spras eq sy-langu
        and   matnr eq it_marc_aux-matnr.

        select *
        from mara
        appending table it_mara_aux
        for all entries in it_marc_aux
        where matnr eq it_marc_aux-matnr.
      endif.
    endif.


    "USER STORY 144142 // MMSILVA - 24.10.2024 - Inicio
*    if it_mchb is initial.
*      if s_matkl is not initial.
*        free: it_mara.
*        select *
*          from mara
*          into table it_mara
*          where matnr in s_matn
*          and matkl in s_matkl.
*
*        free: it_mard.
*        select *
*         from mard
*         into table it_mard
*         for all entries in it_mara
*         where matnr eq it_mara-matnr
*         and   werks in r_werks
*         and   lgort in s_lgor
*         and   labst ne 0.
*
*      endif.

    free: it_mard.
    select *
      from mard
      into table it_mard
      where matnr in s_matn
      and   werks in r_werks
      and   lgort in s_lgor
      and   labst ne 0.

    if it_mard is not initial.
*        free:it_makt.
      select *
       from makt
       appending table it_makt "User Story 144142 // MMSILVA 04.12.2024
       for all entries in it_mard
       where spras eq sy-langu
       and   matnr eq it_mard-matnr.

*        free: it_mara.
      select *
       from mara
       appending table it_mara "User Story 144142 // MMSILVA 04.12.2024
       for all entries in it_mard
       where matnr eq it_mard-matnr.

      free: it_marc.
      select * from marc
        into table it_marc
        for all entries in it_mard
        where matnr eq it_mard-matnr
        and werks eq it_mard-werks.
      if it_marc is not initial.
        sort it_marc by matnr werks.
        delete adjacent duplicates from it_marc comparing matnr werks.
      endif.
*      endif.

      "Saldo em transferencia.
      if s_matkl is not initial.
        select *
          from mara
          appending table it_mara_aux
          where matnr in s_matn
          and matkl   in s_matkl.
        if it_mara_aux is not initial.
          free: it_marc_aux.
          select * from marc
            into table it_marc_aux
            for all entries in it_mara_aux
            where matnr eq it_mara_aux-matnr
            and werks in r_werks
            and ( umlmc ne 0 or trame ne 0 ).
          if it_marc_aux is not initial.
            select *
            from makt
            appending table it_makt
            for all entries in it_marc_aux
            where spras eq sy-langu
            and   matnr eq it_marc_aux-matnr.
          endif.
        endif.

      else.

        free: it_marc_aux.
        select * from marc
          into table it_marc_aux
          where matnr in s_matn
            and werks in r_werks
            and ( umlmc ne 0 or trame ne 0 ).
        if it_marc_aux is not initial.
          select *
          from makt
          appending table it_makt
          for all entries in it_marc_aux
          where spras eq sy-langu
          and   matnr eq it_marc_aux-matnr.

          select *
          from mara
          appending table it_mara_aux
          for all entries in it_marc_aux
          where matnr eq it_marc_aux-matnr.
        endif.
      endif.

      sort it_marc by matnr werks.
      sort it_mard by matnr werks.


      loop at it_mard into wa_mard.

        clear: wa_mara, wa_makt, wa_marc, wa_saida, wa_makt.

        read table it_mchb into wa_mchb with key matnr = wa_mard-matnr.
        if sy-subrc = 0.
          continue.
        endif.

        read table it_mara into wa_mara with key matnr = wa_mard-matnr.

        read table it_marc into wa_marc with key matnr = wa_mchb-matnr
                                                         werks = wa_mard-werks.

        read table it_makt into wa_makt with key matnr = wa_mard-matnr. "USER STORY 144142 // MMSILVA - 04.12.2024

        wa_saida-matnr = wa_mard-matnr.
        wa_saida-maktx = wa_makt-maktx.
        wa_saida-mtart = wa_mara-mtart.
        wa_saida-matkl = wa_mara-matkl.
        wa_saida-meins = wa_mara-meins.
        wa_saida-werks = wa_mard-werks.
        wa_saida-lgort = wa_mard-lgort.
        wa_saida-clabs = wa_mard-labst.

        if wa_marc-umlmc is not initial or wa_marc-trame is not initial.
          if vg_matnr ne wa_saida-matnr.

            wa_saida-umlmc = wa_marc-umlmc.
            wa_saida-trame = wa_marc-trame.

            vg_matnr = wa_saida-matnr.
            vg_werks = wa_saida-werks.

          endif.
        endif.

        append wa_saida to it_saida.
      endloop.

      clear: vg_matnr, vg_werks.

      sort it_mard by matnr werks.
    endif.
    "USER STORY 144142 // MMSILVA - 24.10.2024 - Fim


    sort it_marc by matnr werks.
    sort it_mchb by matnr werks charg.
    sort it_mslb by matnr werks charg.


    loop at it_mchb into wa_mchb.
      clear: wa_mara, wa_makt, wa_mch1, wa_marc.
      read table it_mara into wa_mara with key matnr = wa_mchb-matnr.
*      IF sy-subrc NE 0.
*        CONTINUE.
*      ENDIF.
      read table it_makt into wa_makt with key matnr = wa_mchb-matnr.
      read table it_mch1 into wa_mch1 with key matnr = wa_mchb-matnr
                                               charg = wa_mchb-charg.

      read table it_zppt0011 into wa_zppt0011 with key charg = wa_mchb-charg.

      read table it_marc into wa_marc with key matnr = wa_mchb-matnr werks = wa_mchb-werks.



*>>>Begin-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
      if p_group is not initial.

        "quando flag do "Agrupar lote por fornecedor" estiver marcado, mandar vazio a coluna LOTE
        data(lv_charg) = wa_mchb-charg.

        clear lv_charg.

      endif.
*<<<End-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo

      wa_saida-matnr = wa_mchb-matnr.
      wa_saida-maktx = wa_makt-maktx.
      wa_saida-mtart = wa_mara-mtart.
      wa_saida-matkl = wa_mara-matkl.
      wa_saida-meins = wa_mara-meins.
      wa_saida-werks = wa_mchb-werks.
      wa_saida-lgort = wa_mchb-lgort.
*>>>Begin-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
*      wa_saida-charg = wa_mchb-charg.
      wa_saida-charg = cond #( when p_group is not initial then lv_charg
                               else wa_mchb-charg ).
*<<<End-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
      wa_saida-hsdat = wa_mch1-hsdat.

*     User Story 144142 // MMSILVA 02.10.2024
      if wa_zppt0011-vfdat is not initial and wa_mch1-vfdat is initial.
        wa_saida-vfdat = wa_zppt0011-vfdat.
        wa_saida-diasr = wa_zppt0011-vfdat - sy-datum.
        clear wa_zppt0011-vfdat.
      elseif wa_zppt0011-vfdat is initial and wa_mch1-vfdat is not initial.
        wa_saida-vfdat = wa_mch1-vfdat.
        wa_saida-diasr = wa_mch1-vfdat - sy-datum.
        clear wa_mch1-vfdat.
      elseif wa_zppt0011-vfdat is not initial and wa_mch1-vfdat is not initial.
*** Stefanini - IR251032 - 02/09/2025 - RBRIBEIRO - Início de Alteração
        wa_saida-vfdat = wa_mch1-vfdat.
*** Stefanini - IR251032 - 02/09/2025 - RBRIBEIRO - Fim de Alteração
*        wa_saida-vfdat = wa_zppt0011-vfdat.
        wa_saida-diasr = wa_zppt0011-vfdat - sy-datum.
        clear wa_zppt0011-vfdat.
      elseif wa_zppt0011-vfdat is initial and wa_mch1-vfdat is initial.
        wa_saida-vfdat = wa_mch1-vfdat.
        wa_saida-diasr = wa_mch1-vfdat.
        clear wa_mch1-vfdat.
      endif.
*     User Story 144142 // MMSILVA 02.10.2024

*>>>Begin-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
      data(lv_zppt0011) = value #( it_zppt0011[ charg = wa_mchb-charg ]-lfabr optional ).

      data(lv_zppt0016) = value #( it_zppt0016[ chargd = wa_mchb-charg ]-zlicha optional ).

**      wa_saida-licha = wa_mch1-licha.
      wa_saida-licha = cond #( when lv_zppt0011   is not initial then lv_zppt0011
                               when wa_mch1-licha is not initial then wa_mch1-licha
                               when lv_zppt0016   is not initial then lv_zppt0016
                               else '' ).
*<<<End-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo

      if wa_saida-diasr lt 0.
        wa_saida-status = 'Vencido'.
      else.
        wa_saida-status = 'Lote OK'.
      endif.
      wa_saida-clabs = wa_mchb-clabs + wa_mchb-cinsm + wa_mchb-cspem.
*------>>> CS0970826 / JAMEDICI --->>
*     WA_SAIDA-LIFNR = WA_MCHB-WERKS.
*<<----- CS0970826 / JAMEDICI <<---
      "Checa se existe no fornecedoR

      loop at it_mslb into wa_mslb where matnr = wa_mchb-matnr and werks = wa_mchb-werks and charg = wa_mchb-charg.
*      READ TABLE it_mslb INTO wa_mslb WITH KEY werks = wa_mchb-werks
*                                               matnr = wa_mchb-matnr
*                                               charg = wa_mchb-charg BINARY SEARCH.

*      IF sy-subrc = 0.
        wa_saida-lifnr = wa_mslb-lifnr.
        wa_saida-lblab = wa_mslb-lblab + wa_mslb-lbins.
*      ENDIF.

        read table it_lfa1 into wa_lfa1 with key lifnr = wa_mslb-lifnr. "User Story 144142 // MMSILVA 02.10.2024


        read table it_mcha into data(ws_mcha) with key matnr = wa_mchb-matnr
                                                       werks = wa_mchb-werks
                                                       charg = wa_mchb-charg.

        if sy-subrc eq 0.
*        wa_saida-hsdat =  ws_mcha-hsdat.
*        wa_saida-vfdat =  ws_mcha-vfdat.
*        wa_saida-charg =  ws_mcha-charg.
        endif.

        if wa_saida-lifnr is not initial and wa_saida-lblab is not initial.
          data(vg_lifnr) = wa_saida-lifnr.
          data(vb_qtde)  = wa_saida-lblab.
          wa_saida-lifnr = ' '.
          wa_saida-lblab = ' '.
          append wa_saida to it_saida.

        endif.

        if vb_qtde is not initial.
          wa_saida-lgort = ' '.
          wa_saida-clabs = ' '.
          wa_saida-lifnr = vg_lifnr.
          wa_saida-name1 = wa_lfa1-name1. "User Story 144142 // MMSILVA 02.10.2024
          wa_saida-lblab = vb_qtde.
          append wa_saida to it_saida.
        endif.

        clear: wa_saida-lifnr, wa_saida-lblab, vb_qtde, vg_lifnr, wa_saida-name1, ws_mcha, wa_mslb, wa_saida-pvprs, wa_saida-salk3.
      endloop.

      if wa_saida-clabs is not initial.
        append wa_saida to it_saida.
      endif.

      if wa_marc-umlmc is not initial or wa_marc-trame is not initial.
        if vg_matnr ne wa_saida-matnr.

          wa_saida-umlmc = wa_marc-umlmc.
          wa_saida-trame = wa_marc-trame.

          vg_matnr = wa_saida-matnr.
          vg_werks = wa_saida-werks.

          clear:
          wa_saida-lgort,
          wa_saida-charg,
          wa_saida-hsdat,
          wa_saida-vfdat,
          wa_saida-diasr,
          wa_saida-licha,
          wa_saida-status,
          wa_saida-clabs,
          wa_saida-lifnr,
          wa_saida-lblab,
          wa_saida-lgort,
          wa_saida-clabs.
          append wa_saida to it_saida.
        endif.
      endif.

      clear: wa_saida, vb_qtde, vg_lifnr, wa_mchb, wa_mslb.
    endloop.
    "

    clear: vg_matnr, vg_werks.

    sort it_mchb by matnr werks charg.
    sort it_mslb by matnr werks charg.

    loop at it_mslb into wa_mslb.

      read table it_mchb into wa_mchb with key matnr = wa_mslb-matnr
                                               werks = wa_mslb-werks
                                               charg = wa_mslb-charg.

      if sy-subrc = 0. "Se ja foi processado
        continue.
      endif.

      clear: wa_mara, wa_makt, wa_makt, wa_mch1, wa_marc.

      read table it_mara into wa_mara with key matnr = wa_mslb-matnr.
*      IF sy-subrc NE 0.
*        CONTINUE.
*      ENDIF.
      read table it_makt into wa_makt with key matnr = wa_mslb-matnr.
      read table it_mch1 into wa_mch1 with key matnr = wa_mslb-matnr
                                               charg = wa_mslb-charg.

      read table it_zppt0011 into wa_zppt0011 with key charg = wa_mslb-charg.

      read table it_marc into wa_marc with key matnr = wa_mslb-matnr werks = wa_mslb-werks.
      wa_saida-matnr = wa_mslb-matnr.
      wa_saida-maktx = wa_makt-maktx.
      wa_saida-matkl = wa_mara-matkl.
      wa_saida-mtart = wa_mara-mtart.
      wa_saida-meins = wa_mara-meins.
      wa_saida-werks = wa_mslb-werks.
      wa_saida-lgort = ''.
*>>>Begin-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
*      wa_saida-charg = wa_mslb-charg.
      wa_saida-charg = cond #( when p_group is not initial then lv_charg
                               else wa_mslb-charg ).
*<<<End-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo

      wa_saida-hsdat = wa_mch1-hsdat.

*     User Story 144142 // MMSILVA 02.10.2024 - Inicio
      if wa_zppt0011-vfdat is not initial and wa_mch1-vfdat is initial.
        wa_saida-vfdat = wa_zppt0011-vfdat.
        wa_saida-diasr = wa_zppt0011-vfdat - sy-datum.
        clear wa_zppt0011-vfdat.
      elseif wa_zppt0011-vfdat is initial and wa_mch1-vfdat is not initial.
        wa_saida-vfdat = wa_mch1-vfdat.
        wa_saida-diasr = wa_mch1-vfdat - sy-datum.
        clear wa_mch1-vfdat.
      elseif wa_zppt0011-vfdat is not initial and wa_mch1-vfdat is not initial.
        wa_saida-vfdat = wa_zppt0011-vfdat.
        wa_saida-diasr = wa_zppt0011-vfdat - sy-datum.
        clear wa_zppt0011-vfdat.
      elseif wa_zppt0011-vfdat is initial and wa_mch1-vfdat is initial.
        wa_saida-vfdat = wa_mch1-vfdat.
        wa_saida-diasr = wa_mch1-vfdat.
        clear wa_mch1-vfdat.
      endif.
*     User Story 144142 // MMSILVA 02.10.2024 - Fim

      wa_saida-licha = wa_mch1-licha.


      if wa_saida-diasr lt 0.
        wa_saida-status = 'Vencido'.
      else.
        wa_saida-status = 'Lote OK'.
      endif.
      wa_saida-clabs = 0.
      wa_saida-lifnr = wa_mslb-werks.
      wa_saida-lifnr = wa_mslb-lifnr.
      wa_saida-lblab = wa_mslb-lblab + wa_mslb-lbins.

      read table it_mcha into ws_mcha with key matnr = wa_mchb-matnr
                                               werks = wa_mchb-werks
                                               charg = wa_mchb-charg.

      if sy-subrc eq 0.
*        wa_saida-hsdat =  ws_mcha-hsdat.
*        wa_saida-vfdat =  ws_mcha-vfdat.
*        wa_saida-charg =  ws_mcha-charg.
      endif.

      if wa_saida-lifnr is not initial and wa_saida-lblab is not initial.
        vg_lifnr = wa_saida-lifnr.
        vb_qtde  = wa_saida-lblab.
        wa_saida-lifnr = ' '.
        wa_saida-lblab = ' '.
        append wa_saida to it_saida.
      endif.


      read table it_lfa1 into wa_lfa1 with key lifnr = wa_mslb-lifnr. "User Story 144142 // MMSILVA 02.10.2024

      if vb_qtde is not initial.
        wa_saida-lgort = ' '.
        wa_saida-clabs = ' '.
        wa_saida-lifnr = vg_lifnr.
        wa_saida-name1 = wa_lfa1-name1. "User Story 144142 // MMSILVA 02.10.2024
        wa_saida-lblab = vb_qtde.
        append wa_saida to it_saida.
      endif.

      if wa_saida-clabs is not initial.
        append wa_saida to it_saida.
      endif.



      if wa_marc-umlmc is not initial or wa_marc-trame is not initial.
        if vg_matnr ne wa_saida-matnr.

          wa_saida-umlmc = wa_marc-umlmc.
          wa_saida-trame = wa_marc-trame.

          vg_matnr = wa_saida-matnr.
          vg_werks = wa_saida-werks.

          clear:
          wa_saida-lgort,
          wa_saida-charg,
          wa_saida-hsdat,
          wa_saida-vfdat,
          wa_saida-diasr,
          wa_saida-licha,
          wa_saida-status,
          wa_saida-clabs,
          wa_saida-lifnr,
          wa_saida-lblab,
          wa_saida-lgort,
          wa_saida-clabs.

          append wa_saida to it_saida.
        endif.
      endif.



      clear: wa_saida, vb_qtde, vg_lifnr, wa_saida-name1, wa_marc, wa_mslb, wa_mchb.
    endloop.

    if it_marc_aux is not initial.
      "Saldo em transferencia.
      clear: wa_marc.
      loop at it_marc_aux into wa_marc.
        read table it_mara_aux into wa_mara with key matnr = wa_marc-matnr.
        read table it_makt into wa_makt with key matnr = wa_marc-matnr.

        wa_saida-matnr = wa_marc-matnr.
        wa_saida-maktx = wa_makt-maktx.
        wa_saida-matkl = wa_mara-matkl.
        wa_saida-mtart = wa_mara-mtart.
        wa_saida-meins = wa_mara-meins.
*        wa_saida-status = 'Lote OK'.
        wa_saida-clabs = 0.
        wa_saida-werks = wa_marc-werks.
        wa_saida-umlmc = wa_marc-umlmc.
        wa_saida-trame = wa_marc-trame.
        append wa_saida to it_saida.
        clear: wa_saida, vb_qtde, vg_lifnr, wa_marc.
      endloop.
    endif.

    sort it_saida by matnr werks charg.

    loop at it_saida assigning field-symbol(<wa_saida>).
      if ( <wa_saida>-lblab is initial ) and ( <wa_saida>-clabs is initial ) and ( <wa_saida>-umlmc  is initial ) and ( <wa_saida>-trame is initial ).
        <wa_saida>-saldo_z = abap_true.
        " User Story 144142 // MMSILVA 28.10.2024 - Inicio
      elseif ( <wa_saida>-lblab is not initial ) and ( <wa_saida>-umlmc is not initial ) and ( <wa_saida>-trame is not initial ).
        <wa_saida>-saldo_z = abap_false.
      endif.
      " User Story 144142 // MMSILVA 28.10.2024 - Fim
    endloop.

    delete it_saida where saldo_z eq abap_true.

*>>>Begin-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
    if p_group is initial.
      delete adjacent duplicates from it_saida comparing  matnr werks lgort charg clabs lblab hsdat licha umlmc.
      delete adjacent duplicates from it_saida comparing  matnr werks lgort charg clabs lblab hsdat licha trame.
    endif.

*>>>Begin-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo
    if p_group is not initial.

      data(lt_saida_aux) = it_saida.

      clear it_saida.

      loop at lt_saida_aux into data(ls_saida_aux)
           group by ( hsdat = ls_saida_aux-hsdat licha = ls_saida_aux-licha lgort = ls_saida_aux-lgort
                      matnr = ls_saida_aux-matnr charg = ls_saida_aux-charg ) "User Story 144142 // MMSILVA 23.10.2024
           assigning field-symbol(<fs_saida_groups>).

        data(lv_soma_saldo) = conv mchb-clabs( '' ).
        data(lv_soma_lblab) = conv mslb-lblab( '' ).

        loop at group <fs_saida_groups> assigning field-symbol(<fs_saida_group>).

          lv_soma_saldo += <fs_saida_group>-clabs.
          lv_soma_lblab += <fs_saida_group>-lblab.

        endloop.

        <fs_saida_group>-clabs = lv_soma_saldo.
        <fs_saida_group>-lblab = lv_soma_lblab.

        append <fs_saida_group> to it_saida.

      endloop.

    endif.
*<<<End-Stefanini-138085- Apresentar Lote Fabricante defensivos etiquetados 18.06.2024 - Vitor Rienzo


*   User Story 144142 // MMSILVA 04.10.2024
    if it_saida is not initial.
      free: it_ckmlhd, it_ckmlcr.
      select *
        from ckmlhd
        into table it_ckmlhd
        for all entries in it_saida
        where matnr eq it_saida-matnr
        and   bwkey eq it_saida-werks.
    endif.

    if it_ckmlhd is not initial.

      select *
        from ckmlcr
        into table it_ckmlcr
        for all entries in it_ckmlhd
        where kalnr eq it_ckmlhd-kalnr.
    endif.

    sort it_ckmlcr descending by bdatj poper.

    loop at it_saida assigning field-symbol(<_new_values>).
      if <_new_values>-matnr is not initial.

        data aux_matnr    type matnr18.
        clear: aux_matnr.

        call function 'CONVERSION_EXIT_MATN1_INPUT'
          exporting
            input  = <_new_values>-matnr "campo de 400char
          importing
            output = aux_matnr.


*        SELECT SINGLE * FROM ckmlhd WHERE matnr = @aux_matnr AND bwkey = @<_new_values>-werks INTO @DATA(ls_CKMLHD).
        read table it_ckmlhd into data(ls_ckmlhd) with key matnr = aux_matnr
                                                           bwkey = <_new_values>-werks.

        if ls_ckmlhd is not initial.
*          SELECT SINGLE * FROM ckmlcr WHERE kalnr = @ls_CKMLHD-kalnr AND bdatj = @sy-datum(4) AND poper = '1' AND waers = 'USD' INTO @DATA(ls_ckmlcr).
          read table it_ckmlcr into data(ls_ckmlcr) with key kalnr = ls_ckmlhd-kalnr
                                                             waers = 'USD'.

          if ls_ckmlcr is not initial.
            <_new_values>-pvprs = ls_ckmlcr-pvprs.
            if <_new_values>-lblab > 0.
              <_new_values>-salk3 = <_new_values>-lblab * <_new_values>-pvprs.
            elseif <_new_values>-clabs > 0.
              <_new_values>-salk3 = <_new_values>-clabs * <_new_values>-pvprs.
            endif.
          endif.
        endif.

        <_new_values>-matnr = |{ <_new_values>-matnr alpha = out }|.
      endif.

*     User Story 144142 // MMSILVA 23.10.2024 - Inicio
      if <_new_values>-charg is not initial.

        select single *
          from zppt0011
          where matnr = @aux_matnr
          and werks = @<_new_values>-werks
          and lgort = @<_new_values>-lgort
          and charg = @<_new_values>-charg
          into @data(ls_zppt0011).

        if ls_zppt0011-ebeln is not initial.
          select single *
            from ekko
            where ebeln = @ls_zppt0011-ebeln
            into @data(ls_ekko).

          if ls_ekko-lifnr is not initial.
            <_new_values>-lifnr_ped = ls_ekko-lifnr.

            select single *
              from lfa1
              where lifnr = @ls_ekko-lifnr
              into @data(ls_lfa1).

            if ls_lfa1-name1 is not initial.
              <_new_values>-name1_ped = ls_lfa1-name1.
            endif.
          endif.
        endif.
        clear: ls_zppt0011, ls_ekko, ls_lfa1.
      endif.
*     User Story 144142 // MMSILVA 23.10.2024 - Fim


    endloop.
*   User Story 144142 // MMSILVA 04.10.2024


  endmethod.                    "ZIF_DATA_READER~READ_DATA


  method zif_data_reader~generate_grid.
    data: r_monta_fieldcat type ref to lcl_monta_fieldcat.

    create object r_monta_fieldcat.
    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'MATNR'
                                              i_desc_l = 'Material'
                                              i_tam    = 10 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'MAKTX'
                                              i_desc_l = 'Desc. Material'
                                              i_tam    = 30 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'MTART'
                                              i_desc_l = 'Tipo Material'
                                              i_tam    = 10 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.


    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'MEINS'
                                              i_desc_l = 'UM'
                                              i_tam    = 8 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.


    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'WERKS'
                                              i_desc_l = 'Centro'
                                              i_tam    = 8 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.


    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'LGORT'
                                              i_desc_l = 'Deposito'
                                              i_tam    = 10 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'CHARG'
                                              i_desc_l = 'Lote'
                                              i_tam    = 12 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'HSDAT'
                                              i_desc_l = 'Data de produção'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'VFDAT'
                                              i_desc_l = 'Data Val.'
                                              i_tam    = 10 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'DIASR'
                                              i_desc_l = 'Dias Rest.'
                                              i_tam    = 12 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'CLABS'
                                              i_desc_l = 'Saldo'
                                              i_tam    = 12 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.




    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'LIFNR'
                                              i_desc_l = 'Fornecedor Armz.'
                                              i_tam    = 12 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.


    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'LBLAB'
                                              i_desc_l = 'Saldo Forn.'
                                              i_tam    = 12 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'STATUS'
                                              i_desc_l = 'Status'
                                              i_tam    = 15 ).


    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'MATKL'
                                              i_desc_l = 'Grupo de mercadorias'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'LICHA'
                                              i_desc_l = 'Lote Fornecedor'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'UMLMC'
                                              i_desc_l = 'Est.transf (centro a centro)'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.


    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'TRAME'
                                              i_desc_l = 'Estoque em trânsito'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    append wa_fcat to it_fcat.


*   User Story 144142 // MMSILVA - 04.10.2024 - Inicio
    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'NAME1'
                                              i_desc_l = 'Nome Forn. Armz.'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.


    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'PVPRS'
                                              i_desc_l = 'Preço Moeda Forte'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.


    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'SALK3'
                                              i_desc_l = 'Estq. Moeda Forte'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
*   User Story 144142 // MMSILVA - 04.10.2024 - Fim

*   User Story 144142 // MMSILVA - 23.10.2024 - Inicio
    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'LIFNR_PED'
                                              i_desc_l = 'Fornecedor Pedido'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.

    r_monta_fieldcat->set_fieldcat( exporting i_campo  = 'NAME1_PED'
                                              i_desc_l = 'Denom. Forn. Pedido'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( importing e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).

    append wa_fcat to it_fcat.
*   User Story 144142 // MMSILVA - 23.10.2024 - Inicio



    clear wa_evento.
    wa_evento-name = slis_ev_user_command.
    wa_evento-form = 'USER_COMMAND'.
    append wa_evento to it_evento.

    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
        i_callback_program = sy-repid
        is_layout          = wa_layout
        it_fieldcat        = it_fcat
        it_events          = it_evento
        i_save             = 'X'
      tables
        t_outtab           = it_saida
      exceptions
        program_error      = 1
        others             = 2.
  endmethod.                    "ZIF_DATA_READER~GENERATE_GRID
endclass.                    "LCL_SELECIONA_DADOS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_MONTA_FIELDCAT IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_monta_fieldcat implementation.
  method set_fieldcat.
    me->at_campo  = i_campo.
    me->at_desc_l = i_desc_l.
    me->at_tam    = i_tam.
  endmethod.                    "SET_FIELDCAT

  method get_fieldcat.
    e_campo  = me->at_campo.
    e_desc_l = me->at_desc_l.
    e_tam    = me->at_tam.
  endmethod.                    "GET_FIELDCAT
endclass.                    "LCL_MONTA_FIELDCAT IMPLEMENTATION

*----------------------------------------------------------------------*
*       START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.

  data: r_read_data     type ref to lcl_seleciona_dados,
        r_generate_grid type ref to lcl_seleciona_dados.

  perform authority_check.
  perform check_centros.

  check gv_continue eq abap_true and r_werks is not initial.

  create object r_read_data.
  r_read_data->zif_data_reader~read_data( ).

  create object r_generate_grid.
  r_generate_grid->zif_data_reader~generate_grid( ).

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
form user_command using ucomm    like sy-ucomm
                         selfield type kkblo_selfield.

  data: begin of itab occurs 0,
          name(80) type c,
        end of itab,

        vquant(16).

  read table it_saida into wa_saida index selfield-tabindex.

endform.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*& Form authority_check
*&---------------------------------------------------------------------*
form authority_check .

  loop at s_cent assigning field-symbol(<fs_cent>).
    <fs_cent>-low  = |{ <fs_cent>-low alpha = in }|.
    <fs_cent>-high = |{ <fs_cent>-high alpha = in }|.
  endloop.

  select werks, name1, vkorg into table @data(lt_werks) from t001w
    where werks in @s_cent.

  loop at lt_werks into data(wa_werks).
    authority-check object 'ZMM00132'
    id 'WERKS' field wa_werks-werks
    id 'VKORG' field wa_werks-vkorg.

    if sy-subrc eq 0.
      r_werks = value #( base r_werks ( sign = 'I' option = 'EQ' low = wa_werks-werks ) ).
    else.
      append value #( werks = wa_werks-werks name1 = wa_werks-name1 vkorg = wa_werks-vkorg ) to gt_popup.
    endif.
  endloop.
endform.

*&---------------------------------------------------------------------*
*&      Form  INIT_CONTAINER
*&---------------------------------------------------------------------*
form init_container .
  if go_container is not bound.
    create object go_container
      exporting
*       parent                      =
        container_name              = 'CC_CENTROS'
*       style                       =
*       lifetime                    = lifetime_default
*       repid                       =
*       dynnr                       =
*       no_autodef_progid_dynnr     =
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
form init_alv .
  if go_alv_popup is not bound.
    create object go_alv_popup
      exporting
        i_parent          = go_container
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.
    if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  SHOW_POPUP_DATA
*&---------------------------------------------------------------------*
form show_popup_data .

  call method go_alv_popup->set_table_for_first_display
    changing
      it_outtab                     = gt_popup
      it_fieldcatalog               = gt_fieldcat
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
*&---------------------------------------------------------------------*
form build_fcat .
  if gt_fieldcat is initial.
    gs_fcat-fieldname   = 'WERKS'.
    gs_fcat-ref_table   = 'GT_POPUP'.
    gs_fcat-coltext     = 'Centro'.
    append gs_fcat to gt_fieldcat.
    clear gs_fcat.
    gs_fcat-col_opt     = abap_true.
    gs_fcat-fieldname   = 'NAME1'.
    gs_fcat-ref_table   = 'GT_POPUP'.
    gs_fcat-coltext     = 'Descrição'.
    append gs_fcat to gt_fieldcat.
    clear gs_fcat.
    gs_fcat-col_opt     = abap_true.
    gs_fcat-fieldname   = 'VKORG'.
    gs_fcat-ref_table   = 'GT_POPUP'.
    gs_fcat-coltext     = 'Empresa'.
    append gs_fcat to gt_fieldcat.
    clear gs_fcat.
  endif.
endform.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
module user_command_2000 input.

  case sy-ucomm.
    when 'NO' or 'ECAN'.
      gv_continue = abap_false.
    when 'YES'.
      gv_continue = abap_true.
  endcase.
  set screen 0.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  INIT  OUTPUT
*&---------------------------------------------------------------------*
module init output.
  set pf-status 'POPUP'.
  set titlebar 'CENTROS'.
  perform init_container.
  perform init_alv.
  perform build_fcat.
  perform show_popup_data.

endmodule.
*&---------------------------------------------------------------------*
*& Form check_centros
*&---------------------------------------------------------------------*
form check_centros .
  if gt_popup is not initial.
    call screen 1100 starting at 20 5.
  else.
    gv_continue = abap_true.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1100 input.
  case sy-ucomm.
    when 'NO' or 'ECAN'.
      gv_continue = abap_false.
    when 'YES'.
      gv_continue = abap_true.
  endcase.
  set screen 0.
endmodule.
