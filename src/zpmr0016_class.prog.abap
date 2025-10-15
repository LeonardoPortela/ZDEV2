
*&---------------------------------------------------------------------*
*& CLASS EVENTOS DEFINIÇÃO / ALV
class event definition.

  public section.
    methods:
      handle_double_click for event double_click of cl_gui_alv_grid importing e_row e_column sender.
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID.

*    METHODS:
*      HANDLE_DOUBLE_CLICK_ORDEM FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN SENDER.
endclass.




*&---------------------------------------------------------------------*
*& CLASS EVENTOS IMPLEMETAÇÃO / ALV                                           *

data(obj_even) = new event( ).

class event implementation.

  method handle_double_click.
    check e_row-rowtype is initial.
    perform sel_dados_equip using e_row e_column-fieldname.
  endmethod.
endclass.

*&---------------------------------------------------------------------*
*& CLASS ZUTEIS  DEFINITION                                             *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
class zuteis definition.
  public section.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_HIERARCHY                                      *
*&---------------------------------------------------------------------*
    methods: z_checar_equi_hierarchy exporting
                                       e_return type char1.

*-US 158036-26-11-2024-#158036-RJF-Início
*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_HIERARCHY                                      *
*&---------------------------------------------------------------------*

    data: gv_stop.
    methods: z_bloqueio_custos_frotas.
*-US 158036-26-11-2024-#158036-RJF-Fim

*&---------------------------------------------------------------------*
*& METHOD Z_EQUI_HIERARCHY_READ                                        *
*&---------------------------------------------------------------------*

    methods: z_equi_hierarchy_read importing
                                     equipment type equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_STATUS_BAPIS                                      *
*&---------------------------------------------------------------------*
    methods: z_atualiza_status_bapis importing
                                       txt_status type itex132.

*&---------------------------------------------------------------------*
*& METHOD Z_CREATE_COSTCENTER                                          *
*&---------------------------------------------------------------------*
    methods: z_create_costcenter importing
                                   swerk1 type iwerk
                                   swerk2 type iwerk
                                   center type kostl.

    methods: z_create_costcenter_dev importing
                                       swerk1 type iwerk
                                       swerk2 type iwerk
                                       center type kostl.

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_ZEROS                                               *
*&---------------------------------------------------------------------*
    methods: z_delete_zeros changing
                              field type equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_INSERT_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    methods: z_insert_dados_emprestimo importing
                                         equnr                type equnr
                                         swerk                type swerk
                                         iwerk                type iwerk
                                         qt_dias              type numc3
                                         erdat                type sy-datum
                                         uname                type sy-uname
                                         eqktx                type ktx01
                                         numero_nota          type qmnum
                                         ordem_abast          type daufn
                                         ordem_remon          type ilom_ordst
                                         cent_origem          type swerk
                                         local_origem         type tplnr
                                         nota_zpmt5           type qmnum
                                         imobilizado          type anln1
                                         subimobilizado       type anln2
                                         devolucao_automatica type c."Rubenilson - 23.12.24 - US138088

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    methods: z_delete_dados_emprestimo importing
                                         equipment type equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_DT_HR_DEVOLUCAO                                    *
*&---------------------------------------------------------------------*
    methods: z_checar_dt_hr_devolucao exporting
                                        return type char1
                                      changing
                                        it_tab type any table
                                        wa_tab type any.

    methods: z_limpar_tela.


    methods: z_seleciona_local_tranf importing werks type werks
                                     exporting local type tplnr.

  private section.
    data: concac_text type char200.

endclass.                    "ZUTEIS DEFINITION

*&---------------------------------------------------------------------*
*& CLASS ZUTEIS  IMPLEMENTATION                                         *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

class zuteis implementation.

**********************************************************************
*& Descrição: Checar hierarquia dos equipamentos.                   &*
*& Parâmetro: EQUIPMENT, RETURN                                     &*
*& Atributos Globais                                                &*
********************************************************************&*
  method: z_checar_equi_hierarchy.

    clear: it_status_equnr, e_return, wa_saida_emprestimo_equi, wa_hierarchy.
    loop at it_saida_emprestimo_equi into wa_saida_emprestimo_equi.
      wa_saida_emprestimo_equi-equnr = |{ wa_saida_emprestimo_equi-equnr alpha = in }|.
*     Verificar se existe equipamento superior.
      clear wa_hierarchy.
      select single *
      from equz as a
      inner join equi as b on b~equnr eq a~equnr
      into corresponding fields of wa_hierarchy
      where a~equnr eq wa_saida_emprestimo_equi-equnr
        and a~datbi eq '99991231'.
*        AND B~EQTYP EQ 'V'.

      if ( wa_hierarchy-hequi is not initial ).
        select single *
          from eqkt
          into @data(ls_eqkt_)
            where equnr eq @wa_hierarchy-equnr.

        wa_status_equnr-equnr = |{ wa_hierarchy-equnr alpha = out }|. "INFERIOR
        wa_status_equnr-eqktx = ls_eqkt_-eqktx.
        wa_status_equnr-hequi = wa_hierarchy-hequi. "SUPERIOR

        clear ls_eqkt_.
        select single *
       from eqkt
       into ls_eqkt_
         where equnr eq wa_hierarchy-hequi.
        wa_status_equnr-eqktx_ = ls_eqkt_-eqktx.
        wa_status_equnr-eqp_sup = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
        me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

        append wa_status_equnr to it_status_equnr.
        clear: wa_status_equnr.", WA_HIERARCHY, WA_SAIDA_EMPRESTIMO_EQUI.
        return_status = abap_true.

*     Verificar se existe equipamento superior.
        free it_hierarchy.
        select *
        from equz as a
        inner join equi as b on b~equnr eq a~equnr
        into corresponding fields of table it_hierarchy
        where a~hequi eq wa_saida_emprestimo_equi-equnr
          and a~datbi eq '99991231'.

        if ( it_hierarchy is not initial ).
          clear wa_hierarchy.
          loop at it_hierarchy[] into wa_hierarchy where hequi = wa_saida_emprestimo_equi-equnr.
            if sy-subrc = 0.
              select single *
              from eqkt
              into @data(ls_eqkt)
                where equnr eq @wa_hierarchy-equnr.

              wa_status_equnr-equnr = |{ wa_hierarchy-equnr alpha = out }|. "INFERIOR
              wa_status_equnr-eqktx = ls_eqkt-eqktx.
              wa_status_equnr-hequi = wa_hierarchy-hequi. "SUPERIOR

              clear ls_eqkt.
              select single *
             from eqkt
             into ls_eqkt
               where equnr eq wa_hierarchy-hequi.
              wa_status_equnr-eqktx_ = ls_eqkt-eqktx.
              wa_status_equnr-eqp_inf = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
              me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

              append wa_status_equnr to it_status_equnr.
              clear: wa_status_equnr, wa_saida_emprestimo_equi.
              return_status = abap_true.
            endif.
          endloop.
        endif.

        free it_status_hequi.
        move it_status_equnr to it_status_hequi.
        loop at it_status_hequi into wa_status_equnr.
          if wa_status_equnr-eqp_sup eq 'X'.
            wa_status_equnr-hequi = |{ wa_status_equnr-hequi alpha = in }|.
            select *
            from equz as a
            inner join equi as b on b~equnr eq a~equnr
              into corresponding fields of table st_equz
              where a~hequi eq wa_status_equnr-hequi
                and a~datbi eq '99991231'
                and b~eqtyp ne 'V'
                and b~eqtyp ne 'A' "FF - 05.04.2024 - ins
                and b~eqtyp ne '1' "FF - 22.11.2023 - ins
                and b~eqtyp ne '2'
                and b~eqtyp ne '3'
                and b~eqtyp ne '4'.

            loop at st_equz into w_equz where hequi = wa_status_equnr-hequi.
              if sy-subrc = 0.
                clear ls_eqkt.
                select single *
                 from eqkt
                 into ls_eqkt
                   where equnr eq w_equz-equnr.

                wa_status_hequi-equnr = |{ w_equz-equnr alpha = out }|. "INFERIOR
                wa_status_hequi-eqktx = ls_eqkt-eqktx.
                wa_status_hequi-hequi = w_equz-hequi. "SUPERIOR

                clear ls_eqkt.
                select single *
               from eqkt
               into ls_eqkt
                 where equnr eq w_equz-hequi.
                wa_status_hequi-eqktx_ = ls_eqkt-eqktx.
                wa_status_hequi-eqp_inf = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
                me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

                append wa_status_hequi to it_status_equnr.
*                  APPEND WA_STATUS_HEQUI TO IT_STATUS.
                clear: wa_status_hequi, wa_saida_emprestimo_equi, w_equz.

              endif.
            endloop.
          endif.
        endloop.
*        ENDIF.
        continue.
      endif.

      free it_hierarchy.
      select *
      from equz as a
      inner join equi as b on b~equnr eq a~equnr
      into corresponding fields of table it_hierarchy
      where a~hequi eq wa_saida_emprestimo_equi-equnr
        and a~datbi eq '99991231'.
*        AND B~EQTYP EQ 'V'.

      loop at it_hierarchy[] into wa_hierarchy where hequi = wa_saida_emprestimo_equi-equnr.
        if sy-subrc = 0.
          select single *
          from eqkt
          into @data(gs_eqkt)
            where equnr eq @wa_hierarchy-equnr.

          wa_status_equnr-equnr = |{ wa_hierarchy-equnr alpha = out }|. "INFERIOR
          wa_status_equnr-eqktx = gs_eqkt-eqktx.
          wa_status_equnr-hequi = wa_hierarchy-hequi. "SUPERIOR

          clear ls_eqkt.
          select single *
         from eqkt
         into gs_eqkt
           where equnr eq wa_hierarchy-hequi.
          wa_status_equnr-eqktx_ = gs_eqkt-eqktx.
          wa_status_equnr-eqp_inf = abap_true.
        endif.

*-US 158036-26-11-2024-#158036-RJF-Início
        me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

        append wa_status_equnr to it_status_equnr.
        clear: wa_status_equnr, wa_hierarchy, wa_saida_emprestimo_equi.
        return_status = abap_true.
      endloop.

      free it_status_hequi.
      move it_status_equnr to it_status_hequi.
      loop at it_status_hequi into wa_status_equnr.
        if wa_status_equnr-eqp_inf eq 'X'.
          wa_status_equnr-equnr = |{ wa_status_equnr-equnr alpha = in }|.
          select *
          from equz as a
          inner join equi as b on b~equnr eq a~equnr
            into corresponding fields of table st_equz
            where a~hequi eq wa_status_equnr-equnr
              and a~datbi eq '99991231'
              and b~eqtyp ne 'V'
              and b~eqtyp ne 'A' "FF - 05.04.2024 - ins
              and b~eqtyp ne '1' "FF - 22.11.2023 - ins
              and b~eqtyp ne '2'
              and b~eqtyp ne '3'
              and b~eqtyp ne '4'.

          loop at st_equz into w_equz where hequi = wa_status_equnr-equnr.
            if sy-subrc = 0.
              clear ls_eqkt.
              select single *
               from eqkt
               into ls_eqkt
                 where equnr eq w_equz-equnr.

              wa_status_hequi-equnr = |{ w_equz-equnr alpha = out }|. "INFERIOR
              wa_status_hequi-eqktx = ls_eqkt-eqktx.
              wa_status_hequi-hequi = w_equz-hequi. "SUPERIOR

              clear ls_eqkt.
              select single *
             from eqkt
             into ls_eqkt
               where equnr eq w_equz-hequi.
              wa_status_hequi-eqktx_ = ls_eqkt-eqktx.
              wa_status_hequi-eqp_inf = abap_true.

*-US 158036-26-11-2024-#158036-RJF-Início
              me->z_bloqueio_custos_frotas( ).
*-US 158036-26-11-2024-#158036-RJF-Fim

              append wa_status_hequi to it_status_equnr.
              clear: wa_status_hequi, wa_saida_emprestimo_equi, w_equz.

            endif.
          endloop.
        endif.
      endloop.
    endloop.

    sort it_status_equnr by equnr.
    delete adjacent duplicates from it_status_equnr comparing equnr.
    sort it_status_equnr by hequi.

*    Verificar se equipamento ja existe na tabela IT_SAIDA_EMPRESTIMO_EQUI.
    loop at it_saida_emprestimo_equi into data(w_dev).
      loop at it_status_equnr into data(_status) where hequi = w_dev-equnr.
        if _status-eqp_inf is initial.
          delete it_status_equnr index sy-tabix.
          clear: _status.
          continue.
        endif.
      endloop.

      loop at it_status_equnr into data(status) where equnr = w_dev-equnr.
        if status-eqp_sup is initial.
          delete it_status_equnr index sy-tabix.
          clear: status.
          continue.
        endif.
      endloop.
      clear: status, _status.
    endloop.

  endmethod.                    "Z_CHECAR_EQUI_HIERARCHY
*-US 158036-26-11-2024-#158036-RJF-Início
  method: z_bloqueio_custos_frotas.
    if wa_saida_emprestimo_equi-equnr is not initial.

      data: ln_kostln(4)   type n,
            ln_kostlnx(10) type n,
            ln_kostlm(4)   type n,
            ln_kostlmx(10) type n.

      break rfreitas.

      select equnr, eqtyp, eqart, kostl
        up to 1 rows
        from itob " *ITOB-EQTYP - Objetos técnicos PM (EQUI, local de instalação)
        into @data(wa_itob)
        where equnr eq @wa_saida_emprestimo_equi-equnr.
      endselect.
      if sy-subrc is initial.

        if  wa_itob-eqtyp eq '1'
            or wa_itob-eqtyp eq '2'
            or wa_itob-eqtyp eq '3'
            or wa_itob-eqtyp eq '4'.
*            or wa_itob-eqtyp eq 'A'. "FF #190850

          ln_kostlmx = wa_itob-kostl.
          ln_kostlm = ln_kostlmx+6(4).

          select * from zpmt0001
          into table @data(it_zpmt0001)
          where eqtyp eq @wa_itob-eqtyp
            and eqart eq @wa_itob-eqart.
*            AND kostlg EQ @ln_kostlm.

          if sy-subrc is initial.
            sort it_zpmt0001 by eqtyp eqart.
            loop at it_zpmt0001 into data(wa_pmt0001) where eqtyp = wa_itob-eqtyp
                                                        and eqart = wa_itob-eqart.
              ln_kostln  = wa_pmt0001-kostlg.
              if sy-subrc is not initial or ln_kostln ne ln_kostlm.
*              msg01 = 'Verificar categoria informada x centro de custos tabela ZPM0010.'.
*              MESSAGE  msg01 TYPE 'E'.

                if it_status_equnr[] is initial
                 and ( sy-dynnr eq '0200' or sy-dynnr eq '0400' ).
                  message text-e03 type 'I' display like 'E'.
                  gv_stop = abap_on.
                  exit.
                elseif sy-dynnr eq '0300'.
                  wa_status_equnr-status = icon_alert.
                  wa_status_hequi-status = icon_alert.
                  wa_status_equnr-det = text-e03.
                  wa_status_hequi-det = text-e03.
                endif.
              else.
                if sy-dynnr eq '0300'.
                  wa_status_equnr-status = icon_complete.
                  wa_status_hequi-status = icon_complete.
                endif.
              endif.
            endloop.

          else.
            if it_status_equnr[] is initial
             and ( sy-dynnr eq '0200' or sy-dynnr eq '0400' ).
              message text-e03 type 'I' display like 'E'.
              gv_stop = abap_on.
              exit.
            elseif sy-dynnr eq '0300'.
              wa_status_equnr-status = icon_alert.
              wa_status_hequi-status = icon_alert.
              wa_status_equnr-det = text-e03.
              wa_status_hequi-det = text-e03.
            endif.
          endif.

        endif.
      endif.
    endif.
  endmethod.
*-US 158036-26-11-2024-#158036-RJF-Fim
**********************************************************************
*& Descrição: Atualizar status das BAPIS no alv da tela 0200        &*
*& Parâmetro: TXT_STATUS                                            &*
*& Atributos: ME->CONCAC_TEXT                                       &*
********************************************************************&*
  method z_atualiza_status_bapis.

* Atualizar status das BAPIS na ALV, conforme os dados passados para o método.
*    INDICE = INDICE + 1.
*
*    CONCATENATE INDICE TXT_STATUS INTO ME->CONCAC_TEXT SEPARATED BY SPACE.
*
*    WA_STATUS_BAPIS-TXT_STATUS = ME->CONCAC_TEXT.
*    APPEND WA_STATUS_BAPIS TO IT_STATUS_BAPIS.
*
*    STATUS_BAPIS = 'X'.
*
*    CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.

  endmethod.                    "Z_CHECAR_STATUS_BAPIS

**********************************************************************
*& Descrição: Retornar hierarquia dos equipamentos                  &*
*& Parâmetro: EQUIPMENT                                             &*
*& Atributos Globais                                                &*
********************************************************************&*
  method z_equi_hierarchy_read.

* Função retorna as hierarquias dos equipamentos.
* INFERIOR || SUPERIOR

    call function 'EQUI_HIERARCHY_READ'
      exporting
        equipment  = equipment
        level_down = 01
      tables
        hier_tab   = it_hierarchy.

    delete it_hierarchy[] index 1.
*    DELETE IT_HIERARCHY[] WHERE EQTYP NE 'V'.

  endmethod.                    "Z_EQUI_HIERARCHY_READ

**********************************************************************
*& Descrição: Deletar 000000's das variáveis                        &*
*& Parâmetro: FIELD                                                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  method z_delete_zeros.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = field
      importing
        output = field.

  endmethod.                    "Z_DELETE_ZEROS_VAR

**********************************************************************
*& Descrição: Montar centro de custo                                &*
*& Parâmetro: IWERK1 IWERK2 CENTER                                  &*
*& Atributos Globais                                                &*
********************************************************************&*
  method z_create_costcenter.

* Para criar o centro de custo, primeiramente concatenamos '0', em seguida
* os 2 primeiros dígitos do centro destino, '0' novamente, os 2 ultimos
* dígitos do centro destino, e os 4 ultimos dígitos do
* centro origem.

* Exemplo.: Centro atual: 0150210085 -> Centro destino: 0150070085.

    concatenate '0' swerk1(2) '0' swerk2+2(2) center+6(4) into at_costcenter_destino.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Construir centro de custo destino'
                  filial_origem   = center
                  filial_destino  = center
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  endmethod.                    "Z_CREATE_COSTCENTER


**********************************************************************
*& Descrição: Montar centro de custo                                &*
*& Parâmetro: IWERK1 IWERK2 CENTER                                  &*
*& Atributos Globais                                                &*
********************************************************************&*
  method z_create_costcenter_dev.

* Para criar o centro de custo, primeiramente concatenamos '0', em seguida
* os 2 primeiros dígitos do centro destino, '0' novamente, os 2 ultimos
* dígitos do centro destino, e os 4 ultimos dígitos do
* centro origem.

* Exemplo.: Centro atual: 0150210085 -> Centro destino: 0150070085.

    concatenate '0' swerk1(2) '0' swerk2+2(2) center+6(4) into at_costcenter_destino.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'monta centro de custo '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  endmethod.                    "Z_CREATE_COSTCENTER

**********************************************************************
*& Descrição: Inserior dados dos eqptos emprestados na tabela z     &*
*& Parâmetro: EQUNR, SWERK, IWERK, QT_DIAS, ERDAT, UNAME, EQKTX     &*
*             NUMERO_NOTA, ORDEM_ABAST, ORDEM_REMON                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  method z_insert_dados_emprestimo.

* Inserir dados dos equipamentos que estão sendo emprestados na tabela ZEQUI_EMPRESTIMO,
* esse médoto é chamado no final da das BAPIS.

    wa_zequi_emprestimo-equnr       = equnr.
    wa_zequi_emprestimo-swerk       = swerk.
    wa_zequi_emprestimo-iwerk       = iwerk.
    wa_zequi_emprestimo-qt_dias     = qt_dias.
    wa_zequi_emprestimo-erdat       = erdat.
    wa_zequi_emprestimo-uname       = uname.
    wa_zequi_emprestimo-eqktx       = eqktx.
    wa_zequi_emprestimo-notif_no    = numero_nota.
    wa_zequi_emprestimo-standorder  = ordem_abast.
    wa_zequi_emprestimo-settlorder  = ordem_remon.
    wa_zequi_emprestimo-cent_origem = cent_origem.
    wa_zequi_emprestimo-local_origem =  local_origem.
    wa_zequi_emprestimo-notif_zpm5  =  nota_zpmt5.
    wa_zequi_emprestimo-imobilizado = imobilizado.  "FF - 10/04/2024 #137726
    wa_zequi_emprestimo-subimobilizado = subimobilizado.  "FF - 10/04/2024 #137726
    wa_zequi_emprestimo-dt_fim_emprestimo = erdat + qt_dias. " Rubenilson - 23.12.24 - US138088
    wa_zequi_emprestimo-devolucao_automatica = devolucao_automatica." Rubenilson - 23.12.24 - US138088

    modify zequi_emprestimo from wa_zequi_emprestimo.
    commit work.
    wait up to 02 seconds.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'inserir dados ZEQUI_EMPRESTIMO'
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.

    if it_zpmt0079 is not initial.
      modify zpmt0079 from table it_zpmt0079.
      commit work.
    endif.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_INSERT_TABLE_EMPRESTIMO

**********************************************************************
*& Descrição: Deletar dados dos eqptos da tabela z                  &*
*  no momento da devolução                                          &*
*& Parâmetro: EQUNR                                                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  method z_delete_dados_emprestimo.



  endmethod.                    "Z_DELETE_DADOS_EMPRESTIMO

**********************************************************************
*& Descrição: Checar se parâmetros estão preenchidos para devolver  &*
*& Atributos Globais                                                &*
********************************************************************&*
  method: z_checar_dt_hr_devolucao.
    clear: return_status, wa_saida_equi_responsavel, wa_zequi_emprestimo,
           it_zequi_emprestimo.

* Verifica se se foi selecionado algum equipamento.

    loop at it_saida_equi_responsavel into wa_saida_equi_responsavel
      where cbx_devolver = 'X'.

      if sy-subrc is not initial.
        message i836(sd) with text-001.
        return_status = 'X'.
      else.

        select single *
          from zequi_emprestimo
          into wa_zequi_emprestimo
         where equnr = wa_saida_equi_responsavel-equnr.

*     Verifica se foi informada uma data para o eqpto selecionado.

        if wa_saida_equi_responsavel-dt_devolucao is initial.
          message i836(sd) with text-036 display like 'W'.

          return_status = 'X'.

*     Verifica se a data de devolução é maior que a data de empréstimo.

        elseif wa_saida_equi_responsavel-dt_devolucao < wa_zequi_emprestimo-erdat.
          message i836(sd) with text-044 text-045 display like 'W'.

          return_status = 'X'.

*      Verifica se foi informada uma hora para eqpto selecionado.

        elseif wa_saida_equi_responsavel-hr_devolucao is initial.
          message i836(sd) with text-037 display like 'W'.

          return_status = 'X'.

        endif.

      endif.

    endloop.
  endmethod.                    "Z_CHECAR_DT_HR_DEVOLUCAO

  method z_limpar_tela.

    clear: tbx_equipamento, tbx_centro.

    call method obj_alv_0110->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.                    "Z_LIMPAR_TELA

  method z_seleciona_local_tranf.
    data: ws_iflo type iflo.
    "Seleciona o local de transferencia.

    select single tplnr from iflo into local where iwerk eq werks and fltyp eq 'Y'.

  endmethod.                    "Z_LIMPAR_TELA
endclass.                    "ZUTEIS IMPLEMENTATION


*&----------------------------------------------------------------------------*
*& CLASS SELECIONA_DADOS DEFINITION                                           *
*& AUTOR: ENIO JESUS                                                          *
*& 15.07.2015                                                                 *
*&----------------------------------------------------------------------------*
class z_seleciona_dados definition.

  public section.
*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0110                                       *
*&----------------------------------------------------------------------------*
    methods: z_seleciona_dados_tela_0110.
*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0120                                       *
*&----------------------------------------------------------------------------*
    methods: z_seleciona_dados_tela_0120.
*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0130                                       *
*&----------------------------------------------------------------------------*
    methods  z_seleciona_dados_tela_0130.
*&----------------------------------------------------------------------------*
*& METHOD Z_AUTHORITY_CHECK                                                   *
*&----------------------------------------------------------------------------*
    methods: z_authority_check  importing
                                  object type char7
                                  id     type char5
                                  field  type swerk
                                  return type sy-subrc.

*&----------------------------------------------------------------------------*
*& METHOD Z_STATUS_EQUIPAMENTO                                                *
*&----------------------------------------------------------------------------*
    methods: get_status_equnr importing i_equnr  type equnr
                              exporting e_return like abap_false.

    methods: z_detalhes_equipamento importing
                                      i_equnr type equnr.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_EMPRESTADO                                     *
*&---------------------------------------------------------------------*
    methods: z_checar_equi_emprestado importing
                                        msg    type char1
                                        return type char1.


*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_TELA_RESPONSABILIDADE                               *
*&---------------------------------------------------------------------*
    methods: z_atualiza_tela_respons.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_TELA_POS_EMPRESTIMO                               *
*&---------------------------------------------------------------------*
    methods: z_atualiza_tela_emprestimo.

  private section.
    data: wa_return type bapiret2,
          at_return type sy-subrc.

endclass.                    "Z_SELECIONA_DADOS DEFINITION

*&---------------------------------------------------------------------*
*& CLASS SELECIONA_DADOS IMPLEMENTATION                                *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

class z_seleciona_dados implementation.

************************************************************************
*& Descrição: Método de seleção sub-tela 0110                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por nº de equipamento e centro.                        &*
**********************************************************************&*

  method: z_seleciona_dados_tela_0110.
    data: ls_imptt type imptt,
          wa_fleet type fleet.
    clear: it_msg_return, it_equi, it_saida_equi_disponiveis, it_zequi_emprestimo,
           it_msg_return.

*   Busca pelo nº do equipamento.
    if ( tbx_equipamento is not initial ).

      tbx_equipamento = |{ tbx_equipamento alpha = in }|.
      select *
        into corresponding fields of table it_equi
        from equi as a
        inner join equz as b on b~equnr eq a~equnr
        where a~equnr eq tbx_equipamento
         and  a~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ) "FF - 22/11/20023 e 05/04/2024 type A - ins
          and b~iwerk eq tbx_centro
          and b~datbi eq '99991231'.

      delete adjacent duplicates from it_equi.

*   Busca por centro de custo.
    else.

      select *
        into corresponding fields of table it_equi
        from equi as a
        inner join equz as b on b~equnr eq a~equnr
        where b~iwerk eq tbx_centro
          and a~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ) "FF - 22.11.2023 e 05/04/2024 type A - ins
          and b~datbi eq '99991231'.
    endif.

    sort it_equi by equnr.
    delete adjacent duplicates from it_equi.

*    Verificar status do equipamento, se esta ativo.
    loop at it_equi assigning field-symbol(<w_equi>).
      get_status_equnr( exporting i_equnr  = <w_equi>-equnr
                        importing e_return = return_status ).

      if ( return_status eq abap_true ).
        <w_equi>-marc = abap_true.
      endif.
    endloop.

    delete it_equi where marc eq abap_true.

    select *
    from zequi_emprestimo
    into table it_zequi_emprestimo
    for all entries in it_equi
    where equnr = it_equi-equnr.

*   Separando equipamentos disponiveis e equipamento emprestado.
    if it_equi is not initial and tbx_equipamento is not initial.
      loop at it_zequi_emprestimo into wa_zequi_emprestimo where equnr = tbx_equipamento.
        delete it_equi where equnr = wa_zequi_emprestimo-equnr.
      endloop.
    else.
      loop at it_zequi_emprestimo into wa_zequi_emprestimo.
        delete it_equi where equnr = wa_zequi_emprestimo-equnr.
      endloop.
    endif.

*   Pegar descrição do equipamento.
    select *
      into corresponding fields of table it_eqkt
      from eqkt
   for all entries in it_equi
     where equnr = it_equi-equnr.

    if tbx_equipamento is initial.
      loop at it_equi into wa_equi.

*     Pegar centro origem do eqpto.
        call function 'BAPI_EQUI_GETDETAIL'
          exporting
            equipment         = wa_equi-equnr
          importing
            data_specific_exp = wa_data_specific_exp
            data_general_exp  = wa_data_general.


*      Verificar se o equipamento possue equipamento superior.
        free t_equz.
        select single a~iwerk a~equnr b~objnr b~eqtyp a~datbi a~hequi
        from equz as a
        inner join equi as b on b~equnr = a~equnr
        into t_equz
        where a~equnr eq wa_equi-equnr
          and a~iwerk eq tbx_centro
          and a~datbi eq '99991231'
          and b~eqtyp in ( 'A','V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        if t_equz-hequi is not initial.
          wa_saida_equi_disponiveis-eq_sup = abap_true.
          p_eq_sup = abap_true.
        endif.

*         Verfificando se existe equipamento inferior.
        select single *
        from equz as a
        inner join equi as b on b~equnr eq a~equnr
          into corresponding fields of gt_equz
         where a~hequi eq wa_equi-equnr
           and a~iwerk eq tbx_centro
           and a~datbi eq '99991231'
           and b~eqtyp in ( 'A','V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        if gt_equz is not initial.
          wa_saida_equi_disponiveis-eq_inf = abap_true.
        endif.
*

        read table it_eqkt into wa_eqkt with key equnr = wa_equi-equnr.
        if sy-subrc eq 0.

*          Verificar se equipamento possue ponto de medição combustivel.
          clear ls_imptt.
          select single b~atnam
          from imptt as a
          inner join cabn as b on b~atinn eq a~atinn
          inner join equi as c on c~equnr eq wa_equi-equnr and c~objnr eq a~mpobj
          into ls_imptt
            where a~mptyp eq 'M'
              and a~inact eq ' '
              and b~atnam eq 'COMBUSTIVEL'
              and c~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins

          if ls_imptt is not initial.
            wa_saida_equi_disponiveis-cbx_ord_abast = abap_true.

*         Captura os dados do veículo;
            clear wa_fleet.
            select single *
              into corresponding fields of wa_fleet
              from fleet as a
             inner join equi as b on b~equnr = wa_equi-equnr
                                 and a~objnr = b~objnr
              where key_num ne ' ' or tq_combustivel_1 ne '0'.
            if wa_fleet is initial.
              clear wa_msg_return.
              wa_msg_return-msg = |'Equipamento { wa_equi-equnr alpha = out }não possuí capacidade tq. combustível'|.
              append wa_msg_return to it_msg_return.
            endif.
          endif.

          wa_saida_equi_disponiveis-eqktx = wa_eqkt-eqktx.
          wa_saida_equi_disponiveis-equnr = |{ wa_equi-equnr alpha = out }|.
          wa_saida_equi_disponiveis-iwerk = wa_data_general-maintplant.

          append wa_saida_equi_disponiveis to it_saida_equi_disponiveis.
          clear: wa_saida_equi_disponiveis, wa_equi, wa_eqkt, p_eq_sup.
        endif.
      endloop.

      describe table it_saida_equi_disponiveis lines g_tabstrip-qtd1.
      clear: wa_saida_equi_disponiveis, wa_eqkt.

    else.
      clear p_eq_sup.
      loop at it_equi into wa_equi where equnr = tbx_equipamento.

*      Verfificando se existe equipamento superior.
        select single a~iwerk a~equnr b~objnr b~eqtyp a~datbi a~hequi
        from equz as a
        inner join equi as b on b~equnr = a~equnr
        into t_equz
        where a~equnr eq wa_equi-equnr
          and a~iwerk eq tbx_centro
          and a~datbi eq '99991231'
          and b~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        if t_equz-hequi is not initial.
          wa_saida_equi_disponiveis-eq_sup = abap_true.
          p_eq_sup = abap_true.
        endif.

*      Verfificando se existe equipamento inferior.
        select single *
        from equz as a
        inner join equi as b on b~equnr eq a~equnr
          into @data(z_equz)
         where a~hequi eq @wa_equi-equnr
           and a~iwerk eq @tbx_centro
           and a~datbi eq '99991231'
           and b~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A - ins
        if z_equz is not initial.
          wa_saida_equi_disponiveis-eq_inf = abap_true.
        endif.

*     Pegar centro origem do eqpto.
        call function 'BAPI_EQUI_GETDETAIL'
          exporting
            equipment         = wa_equi-equnr
          importing
            data_specific_exp = wa_data_specific_exp
            data_general_exp  = wa_data_general.

**     Verifica se o usuário possuí permissão para a pesquisa informada.
*      AUTHORITY-CHECK OBJECT 'I_SWERK' ID 'SWERK'
*      FIELD WA_DATA_GENERAL-MAINTPLANT.
*
*      IF ( SY-SUBRC IS INITIAL ).
        read table it_eqkt into wa_eqkt with key equnr = wa_equi-equnr.
        if sy-subrc eq 0.

*        Verificar se equipamento possue ponto de medição combustivel.
          clear ls_imptt.
          select single b~atnam
          from imptt as a
          inner join cabn as b on b~atinn eq a~atinn
          inner join equi as c on c~equnr eq wa_equi-equnr and c~objnr eq a~mpobj
          into ls_imptt
            where a~mptyp eq 'M'
              and a~inact eq ' '
              and b~atnam eq 'COMBUSTIVEL'
              and c~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023  e 05/04/2024 type A - ins
          if ls_imptt is not initial.
            wa_saida_equi_disponiveis-cbx_ord_abast = abap_true.

*          Captura os dados do veículo/Se existe capacidade do tanque;
            clear wa_fleet.
            select single *
              into corresponding fields of wa_fleet
              from fleet as a
             inner join equi as b on b~equnr = wa_equi-equnr
                                 and a~objnr = b~objnr
              where key_num ne ' ' or tq_combustivel_1 ne '0'.
            if wa_fleet is initial.
              clear wa_msg_return.
              wa_msg_return-msg = |'Equipamento { wa_equi-equnr alpha = out }não possuí capacidade tq. combustível'|.
              append wa_msg_return to it_msg_return.
            endif.
          endif.

          wa_saida_equi_disponiveis-eqktx = wa_eqkt-eqktx.
          wa_saida_equi_disponiveis-equnr = |{ wa_equi-equnr alpha = out }|.
          wa_saida_equi_disponiveis-iwerk = wa_data_general-maintplant.

          append wa_saida_equi_disponiveis to it_saida_equi_disponiveis.
          clear: wa_saida_equi_disponiveis, wa_equi, wa_eqkt, t_equz, w_equz, gt_equz.
        endif.
      endloop.

      describe table it_saida_equi_disponiveis lines g_tabstrip-qtd1.
      clear: wa_saida_equi_disponiveis, wa_eqkt.
    endif.
  endmethod.                    "Z_BUSCA_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº de eqpto sub-tela 0110      &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_EQUIPM_DISPONIVEIS.

*    CHECK RETURN_STATUS IS INITIAL.
*    Z_STATUS_EQUIPAMENTO( EQUIPMENT = TBX_BUSC_EQUIPAMENTO ).
*    Z_DETALHES_EQUIPAMENTO( EQUIPMENT = TBX_BUSC_EQUIPAMENTO ).

*  ENDMETHOD.                    "Z_TBX_BUSCA_POR_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0110     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_CENTRO_DISPONIVEIS.
*    CLEAR IT_SAIDA_EQUI_DISPONIVEIS.
*
*    SELECT *
*      FROM ZEQUI_EMPRESTIMO
*      INTO TABLE IT_ZEQUI_EMPRESTIMO.
*
*    SELECT *
*      INTO CORRESPONDING FIELDS OF TABLE IT_EQUI
*      FROM EQUI AS A
*     INNER JOIN ITOB AS B ON A~EQUNR = B~EQUNR
*     INNER JOIN JEST AS C ON A~OBJNR = C~OBJNR
*     WHERE A~EQTYP IN ('V','F','U')
*       AND B~DATBI EQ '99991231'
*       AND B~SWERK EQ TBX_BUSC_CENTRO
*       AND C~STAT  NE 'I0076'
*       AND C~STAT  NE 'I0320'
*       AND C~INACT NE 'X'.
*
*    IF ( SY-SUBRC IS NOT INITIAL ).
*      CHECK ITABSTRIP-ACTIVETAB = 'TAB_DISPONIVEL'.
*      MESSAGE S836(SD) WITH TEXT-040 TEXT-041 DISPLAY LIKE 'E'.
*
*    ELSE.
*
*      SORT IT_EQUI.
*      DELETE ADJACENT DUPLICATES FROM IT_EQUI.
*
**   Deletar os equipamentos da tabela de saída que estão emprestados.
*
*      Z_CHECAR_EQUI_EMPRESTADO(    MSG   = ' '
*                                RETURN   = RETURN_STATUS ).
*
*    ENDIF.
*
*  ENDMETHOD.                    "TBX_BUSC_POR_CENTRO

************************************************************************
*& Descrição: Método de seleção sub-tela 0120                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por equipamento e centro de custo.                     &*
**********************************************************************&*
  method z_seleciona_dados_tela_0120.
    refresh: it_saida_equi_emprestados, it_zequi_emprestimo.
    free it_zequi_emprestimo.
*   Busca pelo nº do equipamento
    if ( tbx_equipamento is not initial ).

      select *
        from zequi_emprestimo
  into table it_zequi_emprestimo
       where equnr eq tbx_equipamento
         and cent_origem eq tbx_centro.
    else.

      select *
        from zequi_emprestimo
       into table it_zequi_emprestimo
       where cent_origem eq tbx_centro.
    endif.

*   Emitir mensagem de erro apenas se estiver na aba referênte aos
*   equipamentos emprestados da sub-tela 0120.

*      IF ITABSTRIP-ACTIVETAB = 'TAB_EMPRESTADOS'.
*        MESSAGE S836(SD) WITH TEXT-010 TEXT-034 DISPLAY LIKE 'W'.
*    ENDIF.

**   Busca pelo nº do centro
*    ELSEIF ( S_WERKS IS NOT INITIAL ).
*      TBX_BUSCA_CENTRO_EMPRESTADOS( ).
*    ENDIF.

    loop at it_zequi_emprestimo into wa_zequi_emprestimo.

*     Verifica se o usuário possuí permissão para a pesquisa informada.
*      AUTHORITY-CHECK OBJECT 'I_SWERK' ID 'CENT_ORIGEM'
*      FIELD WA_ZEQUI_EMPRESTIMO-CENT_ORIGEM.

      if ( sy-subrc is initial ).
        wa_saida_equi_emprestados-equnr       = wa_zequi_emprestimo-equnr.
        wa_saida_equi_emprestados-cent_origem = wa_zequi_emprestimo-cent_origem.
*        WA_SAIDA_EQUI_EMPRESTADOS-SWERK       = WA_ZEQUI_EMPRESTIMO-SWERK.
        wa_saida_equi_emprestados-iwerk       = wa_zequi_emprestimo-iwerk.
        wa_saida_equi_emprestados-qt_dias     = wa_zequi_emprestimo-qt_dias.
        wa_saida_equi_emprestados-uname       = wa_zequi_emprestimo-uname.
        wa_saida_equi_emprestados-erdat       = wa_zequi_emprestimo-erdat.
        wa_saida_equi_emprestados-eqktx       = wa_zequi_emprestimo-eqktx.
        append wa_saida_equi_emprestados to it_saida_equi_emprestados.

      else.
        message s836(sd) with text-031 display like 'E'.
        exit.
      endif.
    endloop.

    describe table it_zequi_emprestimo lines g_tabstrip-qtd2.
    clear: wa_saida_equi_emprestados, wa_zequi_emprestimo.
  endmethod.                    "Z_SELECIONA_DADOS_SCREEN_0120

************************************************************************
*& Descrição: Text-box de pesquisa por nº do eqpto sub-tela 0120      &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_EQUIPM_EMPRESTADOS.
*
*    SELECT *
*      FROM ZEQUI_EMPRESTIMO
*INTO TABLE IT_ZEQUI_EMPRESTIMO
*     WHERE EQUNR EQ TBX_EQUIPAMENTO.
*
*    CHECK SY-SUBRC IS NOT INITIAL.
*
**   Emitir mensagem de erro apenas se estiver na aba referênte aos
**   equipamentos emprestados da sub-tela 0120.
*
*    IF ITABSTRIP-ACTIVETAB     = 'TAB_EMPRESTADOS'.
*      MESSAGE S836(SD) WITH TEXT-010 TEXT-034 DISPLAY LIKE 'W'.
*
**   Emitir mensagem de erro apenas se estiver na aba referênte aos
**   equipamentos emprestados da sub-tela 0130.
*
*    ELSEIF ITABSTRIP-ACTIVETAB = 'TAB_RESPONSAVEL'.
*      MESSAGE S836(SD) WITH TEXT-010 TEXT-035 DISPLAY LIKE 'W'.
*    ENDIF.
*
*    ITABSTRIP-ACTIVETAB = 'TAB_DISPONIVEL'.

*  ENDMETHOD.                    "Z_TBX_BUSCA_POR_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0120     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD: TBX_BUSCA_CENTRO_EMPRESTADOS.

*    SELECT *
*      FROM ZEQUI_EMPRESTIMO AS A
*      INTO TABLE IT_ZEQUI_EMPRESTIMO
*     WHERE SWERK EQ TBX_BUSC_CENTRO.
*
*    CHECK SY-SUBRC IS NOT INITIAL.
*
**   Emitir mensagem de erro apenas se estiver na aba referênte aos
**   equipamentos emprestados da sub-tela 0120.
*
*    IF ITABSTRIP-ACTIVETAB = 'TAB_EMPRESTADOS'.
*      MESSAGE S836(SD) WITH TEXT-042 TEXT-041 DISPLAY LIKE 'E'.
*    ENDIF.

*  ENDMETHOD.                    "TBX_BUSCA_POR_CENTRO_0120

************************************************************************
*& Descrição: Método de seleção sub-tela 0130                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por equipamento e centro de custo.                     &*
**********************************************************************&*
  method z_seleciona_dados_tela_0130.
    refresh: it_saida_equi_responsavel, it_zequi_emprestimo.

    data: lt_cellcolor  type lvc_t_scol, "Rubenilson - 23.12.24 - US138088
          wa_cellcolor  type lvc_s_scol, "Rubenilson - 23.12.24 - US138088
          lv_dt_fim_emp type datum. "Rubenilson - 23.12.24 - US138088

*   Busca pelo nº do equipamento.
    if ( tbx_equipamento is not initial ).

      select *
        from zequi_emprestimo
  into table it_zequi_emprestimo
       where equnr eq tbx_equipamento
         and iwerk eq tbx_centro.

*   Busca pelo nº do centro.
    else.
      select *
       from zequi_emprestimo
       into table it_zequi_emprestimo
       where iwerk eq tbx_centro.

    endif.

    loop at it_zequi_emprestimo into wa_zequi_emprestimo.
      if ( sy-subrc is initial ).
        wa_saida_equi_responsavel-equnr       = |{ wa_zequi_emprestimo-equnr alpha = out }|.
*        WA_SAIDA_EQUI_RESPONSAVEL-SWERK       = WA_ZEQUI_EMPRESTIMO-SWERK.
        wa_saida_equi_responsavel-cent_origem = wa_zequi_emprestimo-cent_origem.
        wa_saida_equi_responsavel-iwerk       = wa_zequi_emprestimo-iwerk.
        wa_saida_equi_responsavel-qt_dias     = wa_zequi_emprestimo-qt_dias.
        wa_saida_equi_responsavel-uname       = wa_zequi_emprestimo-uname.
        wa_saida_equi_responsavel-eqktx       = wa_zequi_emprestimo-eqktx.
        wa_saida_equi_responsavel-erdat       = wa_zequi_emprestimo-erdat.
        wa_saida_equi_responsavel-notif_no = wa_zequi_emprestimo-notif_no.
        wa_saida_equi_responsavel-standorder = wa_zequi_emprestimo-standorder.
        wa_saida_equi_responsavel-settlorder = wa_zequi_emprestimo-settlorder.


*** Inicio - Rubenilson - 24.12.24 - US138088
        free: lt_cellcolor.

        lv_dt_fim_emp = wa_zequi_emprestimo-erdat + wa_zequi_emprestimo-qt_dias.
        if lv_dt_fim_emp >= sy-datum.
          wa_cellcolor-fname = 'QT_DIAS'.
          wa_cellcolor-color-col = 5.
          wa_cellcolor-color-int = 1.
          wa_cellcolor-color-inv = 1.

          insert wa_cellcolor into lt_cellcolor index 1.

        else.

          wa_cellcolor-fname = 'QT_DIAS'.
          wa_cellcolor-color-col = 6.
          wa_cellcolor-color-int = 1.
          wa_cellcolor-color-inv = 1.

          insert wa_cellcolor into lt_cellcolor index 1.

        endif.

        wa_saida_equi_responsavel-cellcolor = lt_cellcolor.
*** Fim - Rubenilson - 24.12.24 - US138088

        append wa_saida_equi_responsavel to it_saida_equi_responsavel.

      else.
        message s836(sd) with text-031 display like 'E'.
        exit.
      endif.
    endloop.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = tbx_equipamento
      importing
        output = tbx_equipamento.

    describe table it_zequi_emprestimo lines g_tabstrip-qtd3.
    clear: wa_saida_equi_responsavel, wa_zequi_emprestimo.
  endmethod.                    "Z_SELECIONA_DADOS_SCREEN_0130

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0130     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
*  METHOD TBX_BUSCA_CENTRO_RESPONSAVEL.
*    SELECT *
*      FROM ZEQUI_EMPRESTIMO
*      INTO TABLE IT_ZEQUI_EMPRESTIMO
*     WHERE IWERK EQ TBX_CENTRO.
*
*    CHECK SY-SUBRC IS NOT INITIAL.
*
*    IF ITABSTRIP-ACTIVETAB = 'TAB_RESPONSAVEL'.
*      MESSAGE S836(SD) WITH TEXT-043 TEXT-041 DISPLAY LIKE 'E'.
*    ENDIF.
*  ENDMETHOD.                    "TBX_BUSCA_POR_CENTRO_0130

************************************************************************
*& Descrição.: Verificar se o usuário possuí permissão ao objeto      &*
*& Atributos.: ME->AT_RETURN                                          &*
*& Parâmetros: OBJECT, ID, FIELD                                      &*
**********************************************************************&*
  method z_authority_check.
    clear return_status.

    authority-check object object
    id id field field.

*    CHECK SY-SUBRC IS NOT INITIAL.
*    RETURN_STATUS = 'X'.

  endmethod.                    "Z_AUTHORITY_CHECK

************************************************************************
*& Descrição.: Obter status do equipamento                            &*
*& Atributos.: Globais                                                &*
*& Parâmetros: EQUIPMENT                                              &*
**********************************************************************&*
  method get_status_equnr.
    clear: it_system_status, it_user_status, wa_system_status,
           wa_return, return_status..

    data v_equnr type equnr.

    call function 'BAPI_EQUI_GETSTATUS'
      exporting
        equipment     = i_equnr
        language      = sy-langu
      importing
        return        = wa_return
      tables
        system_status = it_system_status
        user_status   = it_user_status.

    read table it_system_status into wa_system_status index 1.

    if ( wa_system_status-status = 'I0076' or
         wa_system_status-status = 'I0320' ).
      e_return = abap_true.
    endif.
  endmethod.                    "Z_STATUS_EQUIPAMENTO

************************************************************************
*& Descrição.: Verificar se o eqpto está emprestado                   &*
*& Atributos.: Globais                                                &*
*& Parâmetros: MSG, RETURN                                            &*
**********************************************************************&*
  method z_checar_equi_emprestado.
    clear return_status.

    loop at it_zequi_emprestimo into wa_zequi_emprestimo.
      read table it_equi into wa_equi with key equnr = wa_zequi_emprestimo-equnr.

      check sy-subrc is initial.
      delete it_equi where equnr = wa_zequi_emprestimo-equnr.

      if msg eq 'X'.
        check itabstrip-activetab = 'TAB_DISPONIVEIS'.
        message s836(sd) with text-022 wa_zequi_emprestimo-equnr text-032 display like 'W'.
        itabstrip-activetab = 'TAB_EMPRESTADOS'.
      endif.

      return_status = 'X'.
    endloop.
  endmethod.                    "Z_CHECAR_EQUI_EMPRESTADO

************************************************************************
*& Descrição.: Detalhes do equipamento                                &*
*& Atributos.: WA_DATA_SPECIFIC_EXP, WA_RETURN                        &*
*& Parâmetros:                                                        &*
**********************************************************************&*
  method z_detalhes_equipamento.

    call function 'BAPI_EQUI_GETDETAIL'
      exporting
        equipment         = i_equnr
      importing
        data_specific_exp = wa_data_specific_exp
        return            = wa_return.

    if ( wa_data_specific_exp-equicatgry ne 'V' or
         wa_data_specific_exp-equicatgry ne 'A' or "FF - 05.04.2024 - ins
         wa_data_specific_exp-equicatgry ne '1' or "FF - 22.11.2023 - ins
         wa_data_specific_exp-equicatgry ne '2' or
         wa_data_specific_exp-equicatgry ne '3' or
         wa_data_specific_exp-equicatgry ne '4' or
         wa_data_specific_exp-equicatgry ne 'F' ).

      message s836(sd) with text-002 text-003 display like 'W'.
      return_status = 'X'.
    endif.
  endmethod.                    "Z_DETALHES_EQUIPAMENTO

************************************************************************
*& Descrição.: Atualizar tela após empréstimo de eqpto                &*
*& Atributos.:                                                        &*
*& Parâmetros:                                                        &*
**********************************************************************&*
  method z_atualiza_tela_emprestimo.

    "137696 Ajustar ZPM0026 para chamar endpoint de equipamento quando fizer uma transferencia - PSA

    loop at it_saida_emprestimo_equi assigning field-symbol(<send_mobman_emprestimo>).
      if <send_mobman_emprestimo>-equnr is not initial.
        submit zpmr0078 with s_equnr eq <send_mobman_emprestimo>-equnr with s_iwerk eq tbx_centro_destino and return.
      endif.
    endloop.


*   Fazer a busca dos equipamentos que foram emprestados, e mudar para a
*   aba dos equipamentos emprestados.
    clear: tbx_centro_destino, tbx_qt_dias, it_saida_emprestimo_equi.


    z_seleciona_dados_tela_0110( ).
    z_seleciona_dados_tela_0120( ).

*    FREE GT_EXC_BUTTON.

    wa_stable-col = 'X'.
    wa_stable-row = 'X'.
    call method obj_alv_0110->refresh_table_display
      exporting
        is_stable = wa_stable.

    tbx_centro          = tbx_centro.
    itabstrip-activetab = g_tabstrip-tab2.


    message i836(sd) with text-019 display like 'S'.

    leave to screen 0.

  endmethod.                    "Z_ATUALIZA_TELA_POS_OPERACAO

  method z_atualiza_tela_respons.

    "137696 Ajustar ZPM0026 para chamar endpoint de equipamento quando fizer uma transferencia - PSA

    loop at it_saida_dev_equi assigning field-symbol(<send_mobman_devolucao>).
      if <send_mobman_devolucao>-equnr is not initial and <send_mobman_devolucao>-iwerk is not initial.
        submit zpmr0078 with s_equnr eq <send_mobman_devolucao>-equnr with s_iwerk eq <send_mobman_devolucao>-cent_origem and return.
      endif.
    endloop.

*  FAZER A BUSCA DOS EQUIPAMENTOS QUE FORAM DEVOLVIDOS, E MUDAR PARA A
*  ABA DOS EQUIPAMENTOS DEVOLVIDOS.


    z_seleciona_dados_tela_0130( ).


    wa_stable-col = 'X'.
    wa_stable-row = 'X'.
    call method obj_alv_0130->refresh_table_display
      exporting
        is_stable = wa_stable.

    tbx_centro          = tbx_centro.
    itabstrip-activetab = g_tabstrip-tab3.
    message i836(sd) with text-038 text-039 display like 'S'.

    clear: p_dev_por, p_data_dev, p_hora_dev, it_saida_dev_equi.
    leave to screen 0.
  endmethod.
endclass.                    "Z_SELECIONA_DADOS IMPLEMENTATION



*&-----------------------------------------------------------------------------*
*& CLASS ZBAPIS DEFINITION                                                     *
*& AUTOR: ENIO JESUS                                                           *
*& 13.07.2015                                                                  *
*&-----------------------------------------------------------------------------*
class zbapis definition.
  public section.

    data: zlocal_insta  type tplnr,
          zlocal_origem type tplnr.

*-US 158036-26-11-2024-#158036-RJF-Início
    data: gv_stopzb.
*-US 158036-26-11-2024-#158036-RJF-Fim

    types: begin of ty_selc_plan,
             warpl type vimplastat-warpl,
             mptyp type vimplastat-mptyp,
             strat type vimplastat-strat,
             objnr type vimplastat-objnr,
           end of ty_selc_plan.
*&---------------------------------------------------------------------*
*& METHOD INICIA PROCESSO DAS BAPIS                                    *
*&---------------------------------------------------------------------*
    methods: z_iniciar_processo_emprestimo.
    methods: z_iniciar_processo_devolucao.


*&---------------------------------------------------------------------*
*& METHOD LOCALIZAR ID_CENTRO DE TRABALHO                                           *
*&---------------------------------------------------------------------*
    methods: z_localizar_id_centro_trabalho importing
                                                      objty type cr_objty
                                                      werks type werks_d
                                                      arbpl type arbpl
                                            exporting objid type cr_objid.




*&---------------------------------------------------------------------*
*& METHOD NOTA DE EMPRÉSTIMO                                           *
*&---------------------------------------------------------------------*
    methods: z_criar_nota_emprestimo importing
                                       equipment  type equnr
                                       short_text type qmtxt
                                       priority   type priok
                                       code_group type qmgrp
                                       coding     type qmcod
                                       notif_type type char2
                                       maintplant type iwerk
                                       planplant  type iwerk
                                       funct_loc  type tplnr
                                       plangroup  type ingrp
                                       pm_wkctr   type lgwid
                                       tplnr      type tplnr.

    methods: z_close_nota_emprestimo importing
                                       refdate  type sy-datum
                                       reftime  type syuzeit
                                       notif_no type qmnum.


    methods: z_modify_nota_emprestimo importing
                                        equipment type equnr
                                        planplant type iwerk
                                        tplnr     type tplnr
                                        notif_no  type qmnum.



*&---------------------------------------------------------------------*
*& METHOD ORDENS DE MANUTENÇÃO                                         *
*&---------------------------------------------------------------------*
    methods: z_criar_ordens_manutenc importing
                                       order_type   type aufart
                                       short_text   type auftext
                                       planplant    type iwerk
                                       bus_area     type gsber
                                       funct_loc    type tplnr
                                       mn_wk_ctr    type gewrk
                                       plant        type wergw
                                       maintplant   type swerk
                                       loc_bus_area type gsber
                                       plangroup    type ingrp
                                       equipment    type equnr
                                       costcenter   type kostl
                                       pmacttype    type char3
                                       priority     type priok
                                       activity     type vornr
                                       control_key  type steus
                                       description  type ltxa1
                                       create_notif type char01.


    methods: z_criar_ordens_manutenc_get
      returning value(numero_ordem) type char12.

    methods: z_encerrar_todas_ordens importing
                                       equipment  type equnr
                                       standorder type daufn
                                       settlorder type ilom_ordst.

    methods: z_get_data_ordem importing
                                aufnr         type aufnr
                              exporting
                                ws_data_ordem type viaufkst.

    methods: z_criar_ordens_manutenc_dev importing
                                           order_type   type aufart
                                           short_text   type auftext
                                           planplant    type iwerk
                                           bus_area     type gsber
                                           funct_loc    type tplnr
                                           mn_wk_ctr    type gewrk
                                           plant        type wergw
                                           maintplant   type swerk
                                           loc_bus_area type gsber
                                           plangroup    type ingrp
                                           equipment    type equnr
                                           costcenter   type kostl
                                           pmacttype    type char3
                                           priority     type priok
                                           activity     type vornr
                                           control_key  type steus
                                           description  type ltxa1
                                           create_notif type char01.

    methods: z_criar_ordens_manutenc_get_dv
      returning value(numero_ordem) type char12.

    methods: z_encerrar_todas_ordens_dev importing
                                           equipment  type equnr
                                           standorder type daufn
                                           settlorder type ilom_ordst.

    methods: z_equi_superior importing equnr type equnr
                                       iwerk type iwerk exporting
                                       retorn type c.

*&---------------------------------------------------------------------*
*& METHOD MODIFICAR EQUIPAMENTO                                        *
*&---------------------------------------------------------------------*
    methods: z_desinstal_equipamento importing
                                       equipment type equnr
                                       funcloc   type tplnr.

    methods: z_seleciona_local_tranf importing werks type equz-iwerk
                                     exporting local type tplnr.

    methods: z_detalhes_equipamento importing
                                      equipment  type equnr
                                    exporting
                                      standorder type daufn
                                      settlorder type ilom_ordst
                                      costcenter type ekostl.

    methods: z_status_equipamento  importing
                                     equipment type equnr.
    "FF - 10/04/2024 #137726 - inicio
    methods: z_busca_imobilizado importing
                                   equipamento    type equnr
                                   iwerk          type iwerk
                                 exporting
                                   imobilizado    type anln1
                                   subimobilizado type anln2.
    "FF - 10/04/2024 #137726 - fim
    methods: z_modificar_equipamento importing
                                       equipment      type equnr
                                       maintplant     type swerk
                                       bus_area       type gsber
                                       planplant      type iwerk
                                       costcenter     type kostl
                                       work_ctr       type num8
                                       standorder     type daufn
                                       settlorder     type ilom_ordst
                                       tplnr          type tplnr
                                       imobilizado    type anln1  "FF - 10/04/2024 #137726 -
                                       subimobilizado type anln2.  "FF - 10/04/2024 #137726 -

    methods: z_modificar_equipamento_dev importing
                                           equipment      type equnr
                                           maintplant     type swerk
                                           bus_area       type gsber
                                           planplant      type iwerk
                                           costcenter     type kostl
                                           work_ctr       type num8
                                           standorder     type daufn
                                           settlorder     type ilom_ordst
                                           tplnr          type tplnr
                                           imobilizado    type anln1  "FF - 10/04/2024 #137726 -
                                           subimobilizado type anln2.  "FF - 10/04/2024 #137726 -

    methods: z_instalar_equipamento importing
                                      equipment type equnr
                                      swerk     type swerk
                                      tplnr     type tplnr.

    methods: z_instalar_equipamento_dev importing
                                          equipment type equnr
                                          swerk     type swerk
                                          tplnr     type tplnr.
*&---------------------------------------------------------------------*
*& METHOD MODIFICAR PLANO DE MANUTENÇÃO                                *
*&---------------------------------------------------------------------*
    methods: z_selecionar_planos importing
                                   equipment type equnr.

    methods: z_selecionar_planos_dev importing
                                       equipment type equnr.

    methods: z_set_id_equipament  importing
                                    swerk        type swerk
                                    equnr        type equnr
                                  exporting
                                    id_equipment type num8.

    methods: z_set_id_equipament_dev  importing
                                        swerk        type swerk
                                        tplnr        type tplnr
                                      exporting
                                        id_equipment type num8.


    methods: z_modificar_planos importing
                                  equipment type equnr
                                  swerk     type iwerk
                                  gsber     type gsber
                                  gewrk     type num8
                                  tplnr     type tplnr.
*                                  LAUFN     TYPE DAUFN.

    methods: z_modificar_planos_dev importing
                                      equipment type equnr
                                      swerk     type iwerk
                                      gsber     type gsber
                                      gewrk     type num8
                                      tplnr     type tplnr.
*                                  TPLNR     TYPE TPLNR.
*                                  LAUFN     TYPE DAUFN.
*&---------------------------------------------------------------------*
*& ATRIBUTOS DA CLASSE                                                 *
*&---------------------------------------------------------------------*
  private section .
    data: it_methods      type table of bapi_alm_order_method,
          it_header_up    type table of bapi_alm_order_headers_up,
          it_header       type table of bapi_alm_order_headers_i,
          it_operation    type table of bapi_alm_order_operation,
          it_return       type table of bapiret2,
          t_return        type table of bapiret2,
          it_selc_plan    type table of ty_selc_plan,
          it_notification type standard table of bapi2080_1.

    data: wa_methods            type bapi_alm_order_method,
          wa_header             type bapi_alm_order_headers_i,
          wa_operation          type bapi_alm_order_operation,
          wa_data_general       type bapi_itob,
          wa_data_generalx      type bapi_itobx,
          wa_data_specific      type bapi_itob_eq_only,
          wa_data_specificx     type bapi_itob_eq_onlyx,
          wa_return_bapi_eqmt   type bapireturn,
          wa_return             type bapiret2,
          wa_notifheader_export type bapi2080_nothdre,
          wa_notifheader        type bapi2080_nothdri,
          wa_selc_plan          type ty_selc_plan,
          wa_plko               type plko,
          wa_sysstat            type bapi2080_notsti,
          notif_type            type char2.


    data: at_numero_ordem         type char12,
          at_numero_nota          type char12,
          at_numero_ordem_remonta type char12,
          at_numero_ordem_abastec type char12,
          at_id_equipment         type num8.

    data: it_nota type zpmt0020_t.

    data: short_text              type char40.

endclass.                    "ZBAPIS DEFINITION

*&-------------------------------------------------------------------------------*
*& CLASS ZBAPIS IMPLEMENTATION                                                   *
*& AUTOR: ENIO JESUS                                                             *
*& 13.07.2015                                                                    *
*&-------------------------------------------------------------------------------*

class zbapis implementation.

* INICIAR IMPLEMENTAÇÃO DOS MÉTODOS *

*--------------------------------------------------------------------------------*
*                                                                                *
* MÉTODO PARA REALIZAR O EMPRÉSTIMO DO EQUIPAMENTO                               *
*                                                                                *
*--------------------------------------------------------------------------------*
  method z_iniciar_processo_emprestimo.

    data: r_zuteis type ref to zuteis.
    create object r_zuteis.

    loop at it_saida_emprestimo_equi into wa_saida_emprestimo_equi.
      clear: indice.
      perform f_lupa_ using 'Iniciando processo de transferência' wa_saida_emprestimo_equi-equnr space.
      wa_saida_emprestimo_equi-equnr = |{ wa_saida_emprestimo_equi-equnr alpha = in }|.

*-US 158036-26-11-2024-#158036-RJF-Início
      r_zuteis->z_bloqueio_custos_frotas( ).
      if r_zuteis->gv_stop is not initial.
        me->gv_stopzb = r_zuteis->gv_stop.
        exit.
      endif.
*-US 158036-26-11-2024-#158036-RJF-Fim

****************************/DETALHES DO EQUIPAMENTO\*****************************
*--------------------------------------------------------------------------------*

      z_detalhes_equipamento( exporting equipment  = wa_saida_emprestimo_equi-equnr
                              importing standorder = at_data_general-standorder
                                        settlorder = at_data_general-settlorder
                                        costcenter = at_costcenter_origem ).


****************************/CRIAR CENTRO CUSTO DESTINO\**************************
*--------------------------------------------------------------------------------*

      r_zuteis->z_create_costcenter( swerk1 = tbx_centro_destino
                                     swerk2 = tbx_centro_destino
                                     center = at_costcenter_origem ).

*      R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-014 ).

*************************/VERIFICA SE EXISTE EQUIP SUPERIOR\**********************
*--------------------------------------------------------------------------------*
      z_equi_superior( exporting equnr  = wa_saida_emprestimo_equi-equnr
                                 iwerk  = tbx_centro
                       importing retorn = wa_retorn ).



*************************/DESINSTALAR EQUIPAMENTO DA ORIGEM\**********************
*--------------------------------------------------------------------------------*

      if wa_retorn is initial.
        z_desinstal_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                                 funcloc   = |{ tbx_centro }.FRO| ).
      endif.

*-US 158036-11-12-2024-#158036-RJF-Início
      if gw_erro is not initial.
        vg_erro = abap_true.
        exit.
      endif.
*-US 158036-11-12-2024-#158036-RJF-Fim

*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*
      z_localizar_id_centro_trabalho( exporting objty = 'A'
                                                werks = tbx_centro_destino
                                                arbpl = 'OFICINA'
                                      importing objid = id_cent_trabalho ).

*      z_set_id_equipament( EXPORTING        swerk = tbx_centro_destino
*                                            equnr = wa_saida_emprestimo_equi-equnr
*                           IMPORTING id_equipment = me->at_id_equipment ).
      "FF - 10/04/2024 #137726 - inicio
*          z_busca_imobilizado( exporting equipamento    = wa_saida_emprestimo_equi-equnr "FF #190850 - DEL
*                                         iwerk          = tbx_centro_destino "FF #190850
*                               importing imobilizado    = imobilizado "FF #190850
*                                         subimobilizado = subimobilizado ). "FF #190850
      "FF - 10/04/2024 #137726 - fim


      z_modificar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                              maintplant = tbx_centro_destino
                              bus_area   = tbx_centro_destino
                              costcenter = at_costcenter_destino
                               planplant = tbx_centro_destino
                                work_ctr = id_cent_trabalho"me->at_id_equipment
                              standorder = me->at_numero_ordem_abastec
                              settlorder = me->at_numero_ordem_remonta
*                              tplnr      = zlocal_insta ). "FF - 22.11.2023 - del
                              tplnr      = loc_instalacao  "FF - 22.11.2023 - ins
                              imobilizado = imobilizado
                              subimobilizado = subimobilizado ).

*-US 158036-13-12-2024-#158036-RJF-Inicio
      if gw_erro is not initial.
        exit.
      endif.
*-US 158036-13-12-2024-#158036-RJF-Fim

*      R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-017 ).
*
**************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
**************************************************************************************
      select single * from eqkt into @data(wa_eqkt) where equnr =  @wa_saida_emprestimo_equi-equnr.
      select single bukrs
           into @data(lv_empresa)
           from j_1bbranch
           where branch eq @tbx_centro_destino.
      data:v_texto type ort01_anla.

      v_texto = wa_eqkt-eqktx(25).

*-US 142094-08-10-2024-#142094-RJF-Inicio
      select single bukrs
           into @data(lv_empresaw)
           from j_1bbranch
           where branch eq @tbx_centro.
*-US 142094-08-10-2024-#142094-RJF-fim
*z_iniciar_processo_emprestimo.
      if imobilizado is not initial.
        call function 'Z_TRANSFERIR_IMOBILIZADO'
          exporting
            imobilizado = imobilizado
            equnr       = wa_saida_emprestimo_equi-equnr
            kostl       = at_costcenter_destino
            shtxt       = v_texto
            bukrs       = lv_empresa
            gsber       = tbx_centro_destino "tbx_centro.
            bukrsw      = lv_empresaw.  "US 142094-08-10-2024-#142094-RJF

      endif.
**************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
**************************************************************************************


************************/INSTALAR EQUIPAMENTO NO CENTRO DE DESTINO\*****************
*----------------------------------------------------------------------------------*


      z_instalar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                                 swerk  = tbx_centro_destino
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao ). "FF - 22.11.2023 - ins
      "FF - 10/04/2024 #137726 - inicio
*      z_busca_imobilizado( exporting equipamento    = wa_saida_emprestimo_equi-equnr "FF #190850 - DEL
*                                     iwerk          = tbx_centro_destino
*                           importing imobilizado    = imobilizado
*                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                             maintplant = tbx_centro_destino
                             bus_area   = tbx_centro_destino
                             costcenter = at_costcenter_destino
                              planplant = tbx_centro_destino
                               work_ctr = id_cent_trabalho"me->at_id_equipment
                             standorder = me->at_numero_ordem_abastec
                             settlorder = me->at_numero_ordem_remonta
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao  "FF - 22.11.2023 - ins
                             imobilizado = imobilizado
                             subimobilizado = subimobilizado ).

      if wa_saida_emprestimo_equi-cbx_ord_remon eq 'X'.
*************************/ENCERRAR TODAS AS ORDENS DO EQUI\***********************
*--------------------------------------------------------------------------------*

        z_encerrar_todas_ordens( equipment  = wa_saida_emprestimo_equi-equnr
                                 standorder = at_data_general-standorder
                                 settlorder = at_data_general-settlorder ).




******************************/CRIAR ORDEM DE REMONTA\****************************
*--------------------------------------------------------------------------------*

        short_text = text-029.

        z_criar_ordens_manutenc( order_type   = 'ZPM5'
                                 short_text   = short_text
                                 planplant    = tbx_centro_destino
*                                 funct_loc    = zlocal_insta  "FF - 22.11.2023 - del
                                 funct_loc    = loc_instalacao "FF - 22.11.2023 - ins
                                 bus_area     = tbx_centro_destino
                                 mn_wk_ctr    = 'OFICINA'
                                 plant        = tbx_centro_destino
                                 maintplant   = tbx_centro_destino
                                 loc_bus_area = tbx_centro_destino
                                 plangroup    = 'ABS'
                                 equipment    = wa_saida_emprestimo_equi-equnr
                                 costcenter   = at_costcenter_destino
                                 pmacttype    = 'Z03'
                                 priority     = '4'
                                 activity     = '0010'
                                 control_key  = 'PM01'
                                 description  = short_text
                                 create_notif = abap_true ).

        at_numero_ordem_remonta = z_criar_ordens_manutenc_get( ).

        if at_numero_ordem_remonta is initial.
          message 'Ordem de remonta não foi criada verificar centro de custo de destino' type 'I' display like 'E'.
          continue.
        endif.

      endif.

*      R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-015 ).

**************************/CRIAR ORDEM DE ABASTECIMENTO\**************************
*--------------------------------------------------------------------------------*

      if wa_saida_emprestimo_equi-cbx_ord_abast eq 'X'.

        short_text = text-030.
        z_criar_ordens_manutenc( order_type   = 'ZPM6'
                                 short_text   = short_text
                                 planplant    = tbx_centro_destino
*                                 funct_loc    = zlocal_insta  "FF - 22.11.2023 - del
                                 funct_loc    = loc_instalacao "FF - 22.11.2023 - ins
                                 bus_area     = tbx_centro_destino
                                 mn_wk_ctr    = 'OFICINA'
                                 plant        = tbx_centro_destino
                                 maintplant   = tbx_centro_destino
                                 loc_bus_area = tbx_centro_destino
                                 plangroup    = 'ABS'
                                 equipment    = wa_saida_emprestimo_equi-equnr
                                 costcenter   = at_costcenter_destino
                                 pmacttype    = 'Z11'
                                 priority     = '4'
                                 activity     = '0010'
                                 control_key  = 'PM01'
                                 description  = short_text
                                 create_notif = '' ).

        at_numero_ordem_abastec = z_criar_ordens_manutenc_get( ).

        if at_numero_ordem_abastec is initial.
          message 'Ordem de abastecimento não foi criada verificar centro de custo de destino' type 'S' display like 'E'.
          continue.
        endif.
*        R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-016 ).

      endif.


*****************************/CRIAR NOTA DE EMPRÉSTIMO\***************************
*--------------------------------------------------------------------------------*
      if wa_saida_emprestimo_equi-cbx_ord_abast is not initial.
        concatenate text-028 tbx_centro_destino into short_text separated by space.

        z_localizar_id_centro_trabalho( exporting objty = 'A'
                                                  werks = tbx_centro_destino
                                                  arbpl = 'OFICINA'
                                        importing objid = id_cent_trabalho ).
      endif.

*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*
*      Z_SET_ID_EQUIPAMENT( EXPORTING        SWERK = TBX_CENTRO_DESTINO
*                                            EQ
*                           IMPORTING ID_EQUIPMENT = ME->AT_ID_EQUIPMENT ).
      "FF - 10/04/2024 #137726 - inicio
*      z_busca_imobilizado( exporting equipamento    = wa_saida_emprestimo_equi-equnr "FF #190850 - DEL

*                                     iwerk          = tbx_centro_destino
*                           importing imobilizado    = imobilizado
*                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento( equipment = wa_saida_emprestimo_equi-equnr
                              maintplant = tbx_centro_destino
                              bus_area   = tbx_centro_destino
                              costcenter = at_costcenter_destino
                               planplant = tbx_centro_destino
                                work_ctr = id_cent_trabalho"ME->AT_ID_EQUIPMENT
                              standorder = me->at_numero_ordem_abastec
                              settlorder = me->at_numero_ordem_remonta
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao  "FF - 22.11.2023 - ins
                                 imobilizado = imobilizado  "FF - 10/04/2024 #137726
                                 subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726

*-US 158036-13-12-2024-#158036-RJF-Inicio
      if gw_erro is not initial.
        exit.
      endif.
*-US 158036-13-12-2024-#158036-RJF-Fim

      z_criar_nota_emprestimo( equipment  = wa_saida_emprestimo_equi-equnr
                                 short_text = short_text
                                 priority   = '3'
                                 code_group = 'F0000010'
                                 coding     = '0070'
                                 notif_type = 'Z4'
                                 funct_loc  = |{ tbx_centro_destino }.{ 'FRO' }|
                                 plangroup  = 'FRO'
                                 maintplant = tbx_centro_destino
                                 planplant  = tbx_centro_destino
                                 pm_wkctr   = id_cent_trabalho
*                                 tplnr  = zlocal_insta )."FF - 22.11.2023 - del
                                 tplnr  = loc_instalacao ). "FF - 22.11.2023 - ins


************************/MODIFICAR OS PLANOS DO EQUIPAMENTO\************************
*----------------------------------------------------------------------------------*
      if wa_saida_emprestimo_equi-cbx_ord_abast is not initial.
        z_selecionar_planos( equipment = wa_saida_emprestimo_equi-equnr ).

        if not it_selc_plan is initial.

          z_modificar_planos( equipment = wa_saida_emprestimo_equi-equnr
                                  swerk = tbx_centro_destino
                                  gsber = tbx_centro_destino
                                  gewrk = id_cent_trabalho
*                                  tplnr = zlocal_insta ). "ME->AT_ID_EQUIPMENT ).
                                  tplnr = loc_instalacao ). "FF - 22.11.2023 - ins

*                                TPLNR = |{ TBX_CENTRO_DESTINO }.{ 'FRO' }| ).
*                                LAUFN = ME->AT_NUMERO_ORDEM_ABASTEC ).

          r_zuteis->z_atualiza_status_bapis(
            txt_status = text-018 ).
        endif.
      endif.



*********************/INSERIR DADOS DO EMPRÉSTIMO EM TABELA Z\**********************
*----------------------------------------------------------------------------------*
      if wa_saida_emprestimo_equi-cbx_ord_abast is not initial.
        if p_temp eq abap_true.

          "FF - 10/04/2024 #137726 - inicio
          z_busca_imobilizado( exporting equipamento    = wa_saida_emprestimo_equi-equnr
                                         iwerk          = tbx_centro_destino
                               importing imobilizado    = imobilizado
                                         subimobilizado = subimobilizado ).
          "FF - 10/04/2024 #137726 - fim

          r_zuteis->z_insert_dados_emprestimo(
            equnr                = wa_saida_emprestimo_equi-equnr
            eqktx                = wa_saida_emprestimo_equi-eqktx
            swerk                = wa_data_general-maintplant
            iwerk                = tbx_centro_destino
            qt_dias              = tbx_qt_dias
            erdat                = sy-datum
            uname                = sy-uname
            numero_nota          = wa_notifheader_export-notif_no
            ordem_abast          = me->at_numero_ordem_abastec
            ordem_remon          = me->at_numero_ordem_remonta
            cent_origem          = tbx_centro
            local_origem         = me->zlocal_origem
            nota_zpmt5           = ''
            imobilizado          = imobilizado  "FF - 10/04/2024 #137726
            subimobilizado       = subimobilizado "FF - 10/04/2024 #137726
            devolucao_automatica = wa_saida_emprestimo_equi-devolucao_automatica ). " Rubenilson - 24.12.24 - US138088

          delete it_saida_equi_disponiveis where equnr = wa_saida_emprestimo_equi-equnr.
        endif.
      endif.
      clear: id_cent_trabalho, at_costcenter_destino, at_costcenter_origem, at_numero_ordem_remonta, at_numero_ordem_abastec,
             at_data_general-standorder, at_data_general-settlorder,
             me->at_numero_ordem.


*** Stefanini - IR198570 - 19/09/2024 - LAZAROSR - Início de Alteração
*      CLEAR: TBX_CENTRO_DESTINO.
*** Stefanini - IR198570 - 19/09/2024 - LAZAROSR - Fim de Alteração

    endloop.

    wa_stable-col = 'X'.
    wa_stable-row = 'X'.
    call method obj_alv_0200->refresh_table_display
      exporting
        is_stable = wa_stable.

*    R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-019  ).



  endmethod.                    "CREATE_ZBAPIS

*--------------------------------------------------------------------------------*
*                                                                                *
* MÉTODO PARA REALIZAR A DEVOLUÇÃO DO EQUIPAMENTO                                *
*                                                                                *
*--------------------------------------------------------------------------------*
  method z_iniciar_processo_devolucao.

    clear: vg_erro.
    loop at it_saida_dev_equi into wa_saida_dev_equi.


      wa_saida_dev_equi-equnr = |{ wa_saida_dev_equi-equnr alpha = in }|.

      perform f_lupa using 'Iniciando devolução' wa_saida_dev_equi-equnr space.
      data: r_zuteis                      type ref to zuteis,
            r_atualiza_tela_pos_devolucao type ref to z_seleciona_dados.
      wa_saida_dev_equi-equnr = |{ wa_saida_dev_equi-equnr alpha = in }|.

      create object: r_atualiza_tela_pos_devolucao,
                         r_zuteis.

*-US 158036-11-12-2024-#158036-RJF-Início
      if wa_saida_emprestimo_equi-equnr is initial.
        wa_saida_emprestimo_equi-equnr = wa_saida_dev_equi-equnr.
      endif.
      r_zuteis->z_bloqueio_custos_frotas( ).

      if r_zuteis->gv_stop is not initial.
        vg_erro = r_zuteis->gv_stop.
        exit.
      endif.
*-US 158036-11-12-2024-#158036-RJF-Fim

      select single *
      from zequi_emprestimo
      into @data(zequi_emprestimo)
        where equnr eq @wa_saida_dev_equi-equnr.


      if zequi_emprestimo-local_origem is not initial.
*      CLEAR: zlocal_insta.
        me->zlocal_insta = zequi_emprestimo-local_origem.
      else.
        if me->zlocal_insta is initial.
          z_seleciona_local_tranf(
            exporting
              werks = wa_saida_dev_equi-cent_origem
            importing
              local = me->zlocal_insta
          ).

        endif.
      endif.

      if me->zlocal_insta is initial.
        message i024(sd) with 'Não existe local de intalação tipo ' 'Y cadastrado, não é possivel realizar devolução.'.
        vg_erro = 'X'.
        exit.
      endif.



****************************/DETALHES DO EQUIPAMENTO\*****************************
*--------------------------------------------------------------------------------*

      z_detalhes_equipamento( exporting equipment  = wa_saida_dev_equi-equnr
                              importing costcenter = at_costcenter_origem ).


****************************/CRIAR CENTRO CUSTO DESTINO\**************************
*--------------------------------------------------------------------------------*
      r_zuteis->z_create_costcenter_dev( swerk1 = wa_saida_dev_equi-cent_origem
                                         swerk2 = wa_saida_dev_equi-cent_origem
                                         center = at_costcenter_origem ).



*************************/DESINSTALAR EQUIPAMENTO DA ORIGEM\**********************
*--------------------------------------------------------------------------------*

      z_desinstal_equipamento( equipment = wa_saida_dev_equi-equnr
                               funcloc   = |{ wa_saida_dev_equi-iwerk }.FRO | ).

*-US 158036-11-12-2024-#158036-RJF-Início
      if gw_erro is not initial.
        vg_erro = abap_true.
        exit.
      endif.
*-US 158036-11-12-2024-#158036-RJF-Fim


*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*
      z_localizar_id_centro_trabalho( exporting objty = 'A'
                                                werks = wa_saida_dev_equi-cent_origem
                                                arbpl = 'OFICINA'
                                      importing objid = id_cent_trabalho ).



*
*      z_set_id_equipament_dev( EXPORTING swerk        = wa_saida_dev_equi-cent_origem
*                           IMPORTING id_equipment     = me->at_id_equipment ).
      "FF - 10/04/2024 #137726 - inicio
*      z_busca_imobilizado( exporting equipamento    = wa_saida_dev_equi-equnr "FF #190850 - DEL
*                                     iwerk          = wa_saida_dev_equi-cent_origem
*                           importing imobilizado    = imobilizado
*                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento_dev( equipment      = wa_saida_dev_equi-equnr
                                   maintplant     = wa_saida_dev_equi-cent_origem
                                   bus_area       = wa_saida_dev_equi-cent_origem
                                   costcenter     = at_costcenter_destino
                                   planplant      = wa_saida_dev_equi-cent_origem
                                   work_ctr       = id_cent_trabalho "me->at_id_equipment
                                   standorder     = ''
                                   settlorder     = ''
                                   tplnr          = me->zlocal_insta
                                   imobilizado    = imobilizado  "FF - 10/04/2024 #137726
                                   subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726


***********************/INSTALAR EQUIPAMENTO NO LOCAL ORIGEM\***********************
*----------------------------------------------------------------------------------*
      z_instalar_equipamento_dev( equipment = wa_saida_dev_equi-equnr
                                  swerk     = wa_saida_dev_equi-cent_origem
                                  tplnr     = me->zlocal_insta ).

      "FF - 10/04/2024 #137726 - inicio
      z_busca_imobilizado( exporting equipamento    = wa_saida_dev_equi-equnr
                                     iwerk          = wa_saida_dev_equi-cent_origem
                           importing imobilizado    = imobilizado
                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim
*************************************************************************************
*Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
*************************************************************************************
      select single * from eqkt into @data(wa_eqkt) where equnr =  @wa_saida_emprestimo_equi-equnr.
      data: v_texto type ort01_anla.

      v_texto = wa_eqkt-eqktx(25).
      select single bukrs
           into @data(lv_empresa)
           from j_1bbranch
           where branch eq @tbx_centro_destino.

*-US 142094-08-10-2024-#142094-RJF-Inicio
      select single bukrs
           into @data(lv_empresaw)
           from j_1bbranch
           where branch eq @tbx_centro.
*-US 142094-08-10-2024-#142094-RJF-fim

      if imobilizado is not initial.
        call function 'Z_TRANSFERIR_IMOBILIZADO'
          exporting
            imobilizado = imobilizado
            equnr       = wa_saida_dev_equi-equnr
            kostl       = at_costcenter_destino
            shtxt       = v_texto
            bukrs       = lv_empresa
            gsber       = tbx_centro_destino
            bukrsw      = lv_empresaw.  "US 142094-08-10-2024-#142094-RJF
      endif.
*************************************************************************************
*Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
*************************************************************************************

      z_modificar_equipamento_dev( equipment      = wa_saida_dev_equi-equnr
                                   maintplant     = wa_saida_dev_equi-cent_origem
                                   bus_area       = wa_saida_dev_equi-cent_origem
                                   costcenter     = at_costcenter_destino
                                   planplant      = wa_saida_dev_equi-cent_origem
                                   work_ctr       = id_cent_trabalho "me->at_id_equipment
                                   standorder     = ''
                                   settlorder     = ''
                                   tplnr          = me->zlocal_insta
                                   imobilizado    = imobilizado  "FF - 10/04/2024 #137726
                                   subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726


******************************/CRIAR ORDEM DE REMONTA\****************************
*--------------------------------------------------------------------------------*
      if zequi_emprestimo is not initial.


*************************/ENCERRAR TODAS AS ORDENS DO EQUI\***********************
*--------------------------------------------------------------------------------*


        short_text = text-029.
        z_criar_ordens_manutenc_dev( order_type   = 'ZPM5'
                                     short_text   = short_text
                                     planplant    = wa_saida_dev_equi-cent_origem
                                     bus_area     = wa_saida_dev_equi-cent_origem
                                     funct_loc    = zlocal_insta
                                     mn_wk_ctr    = 'OFICINA'
                                     plant        = wa_saida_dev_equi-cent_origem
                                     maintplant   = wa_saida_dev_equi-cent_origem
                                     loc_bus_area = wa_saida_dev_equi-cent_origem
                                     plangroup    = 'ABS'
                                     equipment    = wa_saida_dev_equi-equnr
                                     costcenter   = at_costcenter_destino
                                     pmacttype    = 'Z03'
                                     priority     = '4'
                                     activity     = '0010'
                                     control_key  = 'PM01'
                                     description  = short_text
                                     create_notif = abap_true ).

        at_numero_ordem_remonta = z_criar_ordens_manutenc_get_dv( ).


**************************/CRIAR ORDEM DE ABASTECIMENTO\**************************
*--------------------------------------------------------------------------------*

        if wa_saida_dev_equi-standorder is not initial.
          short_text = text-030.
          z_criar_ordens_manutenc_dev( order_type   = 'ZPM6'
                                       short_text   = short_text
                                       planplant    = wa_saida_dev_equi-cent_origem
                                       bus_area     = wa_saida_dev_equi-cent_origem
                                       funct_loc    = zlocal_insta
                                       mn_wk_ctr    = 'OFICINA'
                                       plant        = wa_saida_dev_equi-cent_origem
                                       maintplant   = wa_saida_dev_equi-cent_origem
                                       loc_bus_area = wa_saida_dev_equi-cent_origem
                                       plangroup    = 'ABS'
                                       equipment    = wa_saida_dev_equi-equnr
                                       costcenter   = at_costcenter_destino
                                       pmacttype    = 'Z11'
                                       priority     = '4'
                                       activity     = '0010'
                                       control_key  = 'PM01'
                                       description  = short_text
                                       create_notif = ' ' ).

          at_numero_ordem_abastec = z_criar_ordens_manutenc_get_dv( ).
        endif.

        if zequi_emprestimo-standorder is not initial
         or zequi_emprestimo-settlorder is not initial.
          z_encerrar_todas_ordens( equipment  = wa_saida_dev_equi-equnr
                                   standorder = wa_saida_dev_equi-standorder
                                   settlorder = wa_saida_dev_equi-settlorder ).
        endif.
      endif.

*    LOOP AT IT_SAIDA_EQUI_RESPONSAVEL INTO WA_SAIDA_EQUI_RESPONSAVEL WHERE
*      CBX_DEVOLVER = 'X'.




****************************/SELECIONA LOCAL DE TRANSFERENCIA\*****************************
*--------------------------------------------------------------------------------*




****************************/DELETAR DA TABELA DE EMPRESTIMO O EQUIPAMENTO**************************
*--------------------------------------------------------------------------------*
      if zequi_emprestimo is not initial.
        delete from zequi_emprestimo where equnr = wa_saida_dev_equi-equnr.
        commit work.
        wait up to 02 seconds.
      endif.





*      CALL FUNCTION 'BUFFER_SVR_REFRESH'
*        EXPORTING
*          pi_buffer_svr_refresh = abap_true.






***********************/INSTALAR EQUIPAMENTO NO LOCAL ORIGEM\***********************
*----------------------------------------------------------------------------------*

*    Z_INSTALAR_EQUIPAMENTO( EQUIPMENT = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
*                                SWERK = WA_SAIDA_EQUI_RESPONSAVEL-SWERK ).
**    ENDLOOP.




*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*

*      z_set_id_equipament_dev( EXPORTING swerk        = wa_saida_dev_equi-cent_origem
*                           IMPORTING id_equipment     = me->at_id_equipment ).
      "FF - 10/04/2024 #137726 - inicio
*      z_busca_imobilizado( exporting equipamento    = wa_saida_dev_equi-equnr  --> FF #190850 - DEL
*                                     iwerk          = wa_saida_dev_equi-cent_origem
*                           importing imobilizado    = imobilizado
*                                     subimobilizado = subimobilizado ).
      "FF - 10/04/2024 #137726 - fim

      z_modificar_equipamento_dev( equipment      = wa_saida_dev_equi-equnr
                                   maintplant     = wa_saida_dev_equi-cent_origem
                                   bus_area       = wa_saida_dev_equi-cent_origem
                                   costcenter     = at_costcenter_destino
                                   planplant      = wa_saida_dev_equi-cent_origem
                                   work_ctr       = id_cent_trabalho "me->at_id_equipment
                                   standorder     = me->at_numero_ordem_abastec
                                   settlorder     = me->at_numero_ordem_remonta
                                   tplnr          = me->zlocal_insta
                                   imobilizado    = imobilizado  "FF - 10/04/2024 #137726
                                   subimobilizado = subimobilizado ).  "FF - 10/04/2024 #137726



************************/MODIFICAR OS PLANOS DO EQUIPAMENTO\************************
*----------------------------------------------------------------------------------*

      z_selecionar_planos_dev( equipment = wa_saida_dev_equi-equnr ).
      if ( not it_selc_plan is initial ).

        z_modificar_planos_dev( equipment = wa_saida_dev_equi-equnr
                                swerk     = wa_saida_dev_equi-cent_origem
                                gsber     = wa_saida_dev_equi-cent_origem
                                gewrk     = id_cent_trabalho
                                tplnr     = me->zlocal_insta ). "me->at_id_equipment ).
*                              LAUFN = ME->AT_NUMERO_ORDEM_ABASTEC ).
      endif.


****************************/ENCERRAR NOTA DE EMPRÉSTIMO**************************
*--------------------------------------------------------------------------------*
      if zequi_emprestimo-notif_no is not initial.
        z_close_nota_emprestimo( notif_no = wa_saida_dev_equi-notif_no
                                 refdate  = sy-datum
                                 reftime  = sy-uzeit ).
      endif.

*----------------------------------------------------------------------------------*

      delete it_saida_equi_responsavel where equnr = wa_saida_dev_equi-equnr.
      commit work.

      clear: at_id_equipment, at_costcenter_origem, at_costcenter_destino, wa_data_general, zequi_emprestimo, at_numero_ordem_remonta, at_numero_ordem_abastec,
             me->at_numero_ordem, wa_saida_dev_equi.
    endloop.

    wa_stable-col = 'X'.
    wa_stable-row = 'X'.
    call method obj_alv_0400->refresh_table_display
      exporting
        is_stable = wa_stable.


  endmethod.                    "Z_INICIAR_PROCESSO_DEVOLUCAO

**********************************************************************************
*& Descrição: Localizar ID centro de trabalho responsavel                       &*
**********************************************************************************
  method z_localizar_id_centro_trabalho.

    clear: id_cent_trabalho.

    select single *
    from crhd
    into @data(_crhd)
    where objty eq @objty
      and werks eq @werks
      and arbpl eq @arbpl.

    if _crhd is not initial.
      id_cent_trabalho = _crhd-objid.
    endif.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Localiza centro de trabalho'
                  filial_origem   = werks
                  filial_destino  = werks
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.

**********************************************************************************
*& Descrição: Criar nota de empréstimo de equipamento                           &*
*& Atributo.: WA_NOTIFHEADER, WA_NOTIFHEADER_EXPORT, IT_RETURN, WA_RETURN       &*                                                      *
*& Parâmetro: EQUIPMENT, SHORT_TEXT, PRIORITY, CODE_GROUP, CODING, NOTIF_TYPE   &*
**********************************************************************************
  method z_criar_nota_emprestimo.

    clear: it_return, wa_notifheader, wa_return.


    wa_notifheader-funct_loc  = tplnr.
    wa_notifheader-equipment  = equipment.
    wa_notifheader-short_text = short_text.
*    WA_NOTIFHEADER-PRIORITY   = PRIORITY.
    wa_notifheader-code_group = code_group.
    wa_notifheader-coding     = coding.
    wa_notifheader-maintplant = maintplant.
    wa_notifheader-planplant  = planplant.
    wa_notifheader-plangroup  = plangroup.
    wa_notifheader-pm_wkctr   = pm_wkctr.

    call function 'BAPI_ALM_NOTIF_CREATE'
      exporting
        notif_type         = notif_type
        notifheader        = wa_notifheader
        task_determination = ' '
      importing
        notifheader_export = wa_notifheader_export
      tables
        return             = it_return.

    call function 'BAPI_ALM_NOTIF_SAVE'
      exporting
        number      = wa_notifheader_export-notif_no
      importing
        notifheader = wa_notifheader_export
      tables
        return      = it_return.

    clear: wa_return.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
    wait up to 2 seconds.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Criar nota emprestimo'
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_NOTIF_CREATE

  method: z_close_nota_emprestimo.
    clear: wa_sysstat, wa_return.

    wa_sysstat-langu    = sy-langu.
    wa_sysstat-languiso = sy-langu.
    wa_sysstat-refdate  = refdate.
    wa_sysstat-reftime  = reftime.

    call function 'BAPI_ALM_NOTIF_CLOSE'
      exporting
        number   = notif_no
        syststat = wa_sysstat
      tables
        return   = it_return.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
    wait up to 2 seconds.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'fechar nota emprestimo devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


    if it_zpmt0079 is not initial.
      modify zpmt0079 from table it_zpmt0079.
      commit work.
    endif.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_CLOSE_NOTA_EMPRESTIMO

  method: z_modify_nota_emprestimo.

    data: lt_item_mod         type table of bapi2080_notitemi,
          lt_item_modx        type eaml_bapi2080_notitemi_x_t,
          lt_caus_mod         type table of bapi2080_notcausi,
          lt_caus_modx        type alm_me_bapi2080_notcausi_x_t,
          lt_fact_mod         type table of bapi2080_notactvi,
          lt_fact_modx        type alm_me_bapi2080_notactvi_x_t,
          _notifheader_export type bapi2080_nothdre,
          t_return            type bapiret2_t.


    clear: wa_sysstat, wa_return.
    free: t_return .

    data(_notifheader) =
    value bapi2080_nothdri(
                            funct_loc    = tplnr
*                            equipment    = equipment
                            maintplant   = planplant
                            planplant    = planplant
).

    data(_notifheader_x) =
    value bapi2080_nothdri_x(
                              funct_loc    = abap_true
*                              equipment    = abap_true
                              maintplant   = abap_true
                              planplant    = abap_true

    ).


    call function 'BAPI_ALM_NOTIF_DATA_MODIFY'
      exporting
        number             = notif_no
        notifheader        = _notifheader
        notifheader_x      = _notifheader_x
      importing
        notifheader_export = _notifheader_export
      tables
        notifitem          = lt_item_mod
        notifitem_x        = lt_item_modx
        notifcaus          = lt_caus_mod
        notifcaus_x        = lt_caus_modx
        notifactv          = lt_fact_mod
        notifactv_x        = lt_fact_mod
        return             = t_return.
    if not line_exists( t_return[ type = 'E' ] ).

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = 'X'
        importing
          return = wa_return.
      wait up to 2 seconds.
    else.
    endif.

    clear: _notifheader, _notifheader_x, wa_return.
  endmethod.                    "Z_CLOSE_NOTA_EMPRESTIMO

**********************************************************************************
*& Descrição: Criar novas ordens de manutenção com centro do destino            &*
*& Atributo.: WA_HEADER, WA_OPERATION, IT_HEADER, IT_OPERATION, IT_RETURN,      &*
*&            WA_RETURN,                                                        &*                                                      *
*& Parâmetro: EQUIPMENT, ORDER_TYPE, SHORT_TEXT, PLANPLANT, BUS_AREA, PLANT     &*
*&            MN_WK_CTR, LOC_BUS_AREA, PLANGROUP, COSTCENTER, PRIORITY          &*
*&            ACTIVITY, CONTROL_KEY, DESCRIPTION, AT_NUMERO_ORDEM               &*
**********************************************************************************
  method z_criar_ordens_manutenc.


    data: lv_orderid      type aufnr,
          lv_refnum       type ifrefnum,
          lv_oper_no      type objidext,
          ls_iloa         type iloa,
          numero_ordem_v2 type char1,
          lv_empresa      type bukrs,
          _method         type swo_method,
          at_orderid      type aufnr value 1,
          at_refnum       type ifrefnum value 1,
          at_seqopr       type ifrefnum,
          at_oper_no      type objidext.



    free: it_methods[], it_header[], it_operation[], it_return[], it_header_up.
    clear: wa_return, at_numero_ordem, wa_header, wa_notifheader_export, notif_type, me->at_numero_nota, wa_notifheader_export.

    data: it_nota type zpmt0020_t.

    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - início / AOENNING
    if create_notif eq abap_true.

      clear: id_cent_trabalho.
      me->z_localizar_id_centro_trabalho( exporting objty = 'A'
                                                    werks = planplant
                                                    arbpl = 'OFICINA'
                                          importing objid = id_cent_trabalho ).

      wa_notifheader-funct_loc  = funct_loc.
      wa_notifheader-equipment  = equipment.
      wa_notifheader-short_text = short_text.
      wa_notifheader-priority   = '4'.
      wa_notifheader-code_group = 'F0000010'.
      wa_notifheader-coding     = '0020'.
      wa_notifheader-reportedby = sy-uname.
      wa_notifheader-maintplant = maintplant.
      wa_notifheader-planplant  = planplant.
      wa_notifheader-plangroup  = plangroup.
      wa_notifheader-pm_wkctr   = id_cent_trabalho.
      notif_type = 'Z9'.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      free: it_return.
      call function 'BAPI_ALM_NOTIF_CREATE' "#EC CI_USAGE_OK[2438131]
        exporting
          notif_type         = notif_type
          notifheader        = wa_notifheader
          task_determination = ' '
        importing
          notifheader_export = wa_notifheader_export
        tables
          return             = it_return.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      free: it_return.
      call function 'BAPI_ALM_NOTIF_SAVE' "#EC CI_USAGE_OK[2438131]
        exporting
          number      = wa_notifheader_export-notif_no
        importing
          notifheader = wa_notifheader_export
        tables
          return      = it_return.

      clear: wa_return.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = 'X'
        importing
          return = wa_return.
      wait up to 2 seconds.

    endif.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim


    at_orderid = |{ at_orderid alpha = in }|.
    at_refnum  = |{ at_refnum  alpha = in }|.

    at_orderid+0(1) = '%'.
    _method = 'CREATE'.

    at_oper_no = at_orderid.
    at_oper_no+12(4) = '0010'.

    it_methods = value bapi_alm_order_method_t( ( refnumber = at_refnum objecttype = 'HEADER'    method = _method  objectkey  = at_orderid ) ).
    append value #( refnumber = at_refnum objecttype = 'OPERATION' method = _method  objectkey  = at_oper_no ) to it_methods.
    append value #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE' objectkey = at_orderid ) to it_methods.
    append value #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = at_orderid ) to it_methods.

    if order_type = 'ZPM5'.
      clear it_methods[].
      concatenate at_orderid wa_notifheader_export-notif_no into data(at_notif_no).
      append value #( refnumber = at_refnum objecttype = 'HEADER'    method = 'CREATETONOTIF'  objectkey = at_notif_no ) to it_methods.
      append value #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE'        objectkey = at_orderid  ) to it_methods.
      append value #( refnumber = at_refnum objecttype = ''          method = 'SAVE'           objectkey = at_notif_no ) to it_methods.

      wa_header-orderid = at_orderid.
      wa_header-notif_type = 'Z9'.

      append value #( orderid  = at_orderid
                      notif_no = abap_true )
                to it_header_up.

    endif.

    select single bukrs
      into lv_empresa
      from j_1bbranch
      where branch eq planplant.

*    wa_header-orderid       = lv_orderid.
    wa_header-order_type    = order_type.
    wa_header-funct_loc     = funct_loc.
    wa_header-short_text    = short_text.
    wa_header-planplant     = planplant.
    wa_header-loc_comp_code = lv_empresa.
    wa_header-bus_area      = bus_area.
    wa_header-mn_wk_ctr     = mn_wk_ctr.
    wa_header-plant         = plant.
    wa_header-maintplant    = maintplant.
    wa_header-loc_bus_area  = loc_bus_area.
    wa_header-plangroup     = plangroup.
    wa_header-equipment     = equipment.
    wa_header-costcenter    = costcenter.
    wa_header-start_date    = sy-datum.
    wa_header-priority      = priority.
    append wa_header to it_header.
    clear wa_header.

    wa_operation-activity    = activity.
    wa_operation-control_key = control_key.
    wa_operation-description = description.
    append wa_operation to it_operation.
    clear wa_operation.

    free: it_return.
    call function 'BAPI_ALM_ORDER_MAINTAIN'
      tables
        it_methods   = it_methods
        it_header    = it_header
        it_header_up = it_header_up
        it_operation = it_operation
        return       = it_return.

    clear: wa_return.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
    wait up to 02 seconds.

    read table it_return into wa_return with key number = '112'.

    "14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    if wa_return-message_v2 is initial.
      read table it_return into wa_return with key number = '126'.
    endif.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    clear: me->at_numero_ordem.
    me->at_numero_ordem = wa_return-message_v2.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = me->at_numero_ordem
      importing
        output = me->at_numero_ordem.

    clear: me->at_numero_nota.
    me->at_numero_nota = wa_notifheader_export-notif_no.


*   Verificar local instalação da ordem.
    select single *
    from iloa as a
    inner join afih as b on b~iloan eq a~iloan
    into corresponding fields of ls_iloa
     where b~aufnr eq at_numero_ordem.


    if ls_iloa-tplnr is not initial and ls_iloa-tplnr ne funct_loc.
      perform shdb_mod_ordem using funct_loc me->at_numero_ordem.

      if wa_notifheader_export-notif_no is not initial.
        me->z_modify_nota_emprestimo(
          exporting
            equipment = equipment
            planplant = planplant
            tplnr     = funct_loc
            notif_no  = wa_notifheader_export-notif_no
        ).
      endif.
    endif.

    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    if create_notif eq abap_true.
      "Preenche o numero da ordem na operação
      data: lt_oprol3 type table of bapi_alm_olist_relation,
            lt_return type table of bapiret2.

      free: it_return.
      call function 'BAPI_ALM_ORDER_GET_DETAIL'
        exporting
          number   = me->at_numero_ordem
        tables
          et_oprol = lt_oprol3
          return   = lt_return.

      call function 'ZPM_ATUALIZA_OBJETOS_ORDEM' in background task as separate unit
        exporting
          i_aufnr = me->at_numero_ordem
          i_notas = it_nota "wa_notifheader_export-notif_no
        tables
          i_oprol = lt_oprol3.

    endif.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim


*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Criar ordem manutenção '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_ORDER_MAINTAIN

*  **********************************************************************************
*& Descrição: Verifica nota de manutenção vinculada a ordem&*
*& Atributo.: WA_HEADER, WA_OPERATION, IT_HEADER, IT_OPERATION, IT_RETURN,      &*
*&            WA_RETURN,                                                        &*                                                      *
*& Parâmetro: EQUIPMENT, ORDER_TYPE, SHORT_TEXT, PLANPLANT, BUS_AREA, PLANT     &*
*&            MN_WK_CTR, LOC_BUS_AREA, PLANGROUP, COSTCENTER, PRIORITY          &*
*&            ACTIVITY, CONTROL_KEY, DESCRIPTION, AT_NUMERO_ORDEM               &*
**********************************************************************************
  method  z_get_data_ordem.

    "Verifica se ordem de nota vinculada.
    select single * from viaufkst
      into ws_data_ordem
      where aufnr eq aufnr.

  endmethod.
*  **********************************************************************************
*& Descrição: Criar novas ordens de manutenção com centro do destino            &*
*& Atributo.: WA_HEADER, WA_OPERATION, IT_HEADER, IT_OPERATION, IT_RETURN,      &*
*&            WA_RETURN,                                                        &*                                                      *
*& Parâmetro: EQUIPMENT, ORDER_TYPE, SHORT_TEXT, PLANPLANT, BUS_AREA, PLANT     &*
*&            MN_WK_CTR, LOC_BUS_AREA, PLANGROUP, COSTCENTER, PRIORITY          &*
*&            ACTIVITY, CONTROL_KEY, DESCRIPTION, AT_NUMERO_ORDEM               &*
**********************************************************************************
  method z_criar_ordens_manutenc_dev.

    free: it_methods, it_header, it_operation, it_return.
    clear: wa_return, wa_header, wa_operation, wa_methods.

    data: lv_orderid      type aufnr,
          lv_refnum       type ifrefnum,
          lv_oper_no      type objidext,
          contnum         type p decimals 2,
          ls_iloa         type iloa,
          numero_ordem_v2 type char10,
          lv_empresa      type bukrs,
          _method         type swo_method,
          at_orderid      type aufnr value 1,
          at_refnum       type ifrefnum value 1,
          at_seqopr       type ifrefnum,
          at_oper_no      type objidext.



    free: it_methods[], it_header[], it_operation[], it_return[], it_header_up.
    clear: wa_return, at_numero_ordem, wa_header, wa_notifheader_export, notif_type, me->at_numero_nota, wa_notifheader_export.


    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - início / AOENNING
    if create_notif eq abap_true.

      clear: id_cent_trabalho.
      me->z_localizar_id_centro_trabalho( exporting objty = 'A'
                                                    werks = planplant
                                                    arbpl = 'OFICINA'
                                          importing objid = id_cent_trabalho ).

      wa_notifheader-funct_loc  = funct_loc.
      wa_notifheader-equipment  = equipment.
      wa_notifheader-short_text = short_text.
      wa_notifheader-priority   = '4'.
      wa_notifheader-code_group = 'F0000010'.
      wa_notifheader-coding     = '0020'.
      wa_notifheader-reportedby = sy-uname.
      wa_notifheader-maintplant = maintplant.
      wa_notifheader-planplant  = planplant.
      wa_notifheader-plangroup  = plangroup.
      wa_notifheader-pm_wkctr   = id_cent_trabalho.
      notif_type = 'Z9'.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      free: it_return.
      call function 'BAPI_ALM_NOTIF_CREATE' "#EC CI_USAGE_OK[2438131]
        exporting
          notif_type         = notif_type
          notifheader        = wa_notifheader
          task_determination = ' '
        importing
          notifheader_export = wa_notifheader_export
        tables
          return             = it_return.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      free: it_return.
      call function 'BAPI_ALM_NOTIF_SAVE' "#EC CI_USAGE_OK[2438131]
        exporting
          number      = wa_notifheader_export-notif_no
        importing
          notifheader = wa_notifheader_export
        tables
          return      = it_return.

      clear: wa_return.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = 'X'
        importing
          return = wa_return.
      wait up to 2 seconds.

    endif.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    at_orderid = |{ at_orderid alpha = in }|.
    at_refnum  = |{ at_refnum  alpha = in }|.

    at_orderid+0(1) = '%'.
    _method = 'CREATE'.

    at_oper_no = at_orderid.
    at_oper_no+12(4) = '0010'.

    it_methods = value bapi_alm_order_method_t( ( refnumber = at_refnum objecttype = 'HEADER'    method = _method  objectkey  = at_orderid ) ).
    append value #( refnumber = at_refnum objecttype = 'OPERATION' method = _method  objectkey  = at_oper_no ) to it_methods.
    append value #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE' objectkey = at_orderid ) to it_methods.
    append value #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = at_orderid ) to it_methods.

    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / AOENNING
    if order_type = 'ZPM5'.
      clear it_methods[].
      concatenate at_orderid wa_notifheader_export-notif_no into data(at_notif_no).
      append value #( refnumber = at_refnum objecttype = 'HEADER'    method = 'CREATETONOTIF'  objectkey = at_notif_no ) to it_methods.
      append value #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE'        objectkey = at_orderid  ) to it_methods.
      append value #( refnumber = at_refnum objecttype = ''          method = 'SAVE'           objectkey = at_notif_no ) to it_methods.

      wa_header-orderid = at_orderid.
      wa_header-notif_type = 'Z9'.

      append value #( orderid  = at_orderid
                      notif_no = abap_true )
                to it_header_up.

    endif.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim



    select single bukrs
    into lv_empresa
    from j_1bbranch
    where branch eq planplant.


*    wa_header-orderid       = lv_orderid.
    wa_header-funct_loc     = funct_loc.
    wa_header-order_type    = order_type.
    wa_header-short_text    = short_text.
    wa_header-loc_comp_code = lv_empresa.
    wa_header-planplant     = planplant.
    wa_header-bus_area      = bus_area.
    wa_header-mn_wk_ctr     = mn_wk_ctr.
    wa_header-plant         = plant.
    wa_header-maintplant    = maintplant.
    wa_header-loc_bus_area  = loc_bus_area.
    wa_header-plangroup     = plangroup.
    wa_header-equipment     = equipment.
    wa_header-costcenter    = costcenter.
    wa_header-start_date    = sy-datum.
    wa_header-priority      = priority.
    append wa_header to it_header.
    clear wa_header.

    wa_operation-activity    = activity.
    wa_operation-control_key = control_key.
    wa_operation-description = description.
    append wa_operation to it_operation.
    clear wa_operation.

    free: it_return.
    call function 'BAPI_ALM_ORDER_MAINTAIN'
      tables
        it_methods   = it_methods
        it_header    = it_header
        it_header_up = it_header_up
        it_operation = it_operation
        return       = it_return.

    clear: wa_return.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
    wait up to 2 seconds.

    read table it_return into wa_return with key number = '112'.

    "14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    if wa_return-message_v2 is initial.
      read table it_return into wa_return with key number = '126'.
    endif.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    me->at_numero_ordem = wa_return-message_v2.

*   Adicionando zero a esquerda.
    me->at_numero_ordem = |{ me->at_numero_ordem alpha = in }|.

    clear: me->at_numero_nota.
    me->at_numero_nota = wa_notifheader_export-notif_no.



    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - inicio / aoenning
    if create_notif eq abap_true.
      "Preenche o numero da ordem na operação
      data: lt_oprol3 type table of bapi_alm_olist_relation,
            lt_return type table of bapiret2.

      free: it_return.
      call function 'BAPI_ALM_ORDER_GET_DETAIL'
        exporting
          number   = me->at_numero_ordem
        tables
          et_oprol = lt_oprol3
          return   = lt_return.

      free: it_nota.
      it_nota = value #( ( aufnr = me->at_numero_ordem
                         qmnum = wa_notifheader_export-notif_no ) ).

      call function 'ZPM_ATUALIZA_OBJETOS_ORDEM' in background task as separate unit
        exporting
          i_aufnr = me->at_numero_ordem
          i_notas = it_nota "wa_notifheader_export-notif_no
        tables
          i_oprol = lt_oprol3.

    endif.
    "#14/09/2023 - Ajustes Ordens tipo ZPM5 - fim


*   Verificar local instalação da ordem.
    select single *
    from iloa as a
    inner join afih as b on b~iloan eq a~iloan
    into corresponding fields of ls_iloa
     where b~aufnr eq at_numero_ordem.


    if ls_iloa-tplnr is not initial and ls_iloa-tplnr ne funct_loc.

      "FF - 11/04/2024 #137726 - inicio
      if wa_notifheader_export-notif_no is not initial.
        perform shdb_modif_notif using wa_notifheader_export-notif_no.
      endif.
      "FF - 11/04/2024 #137726 - fim


      perform shdb_mod_ordem using funct_loc me->at_numero_ordem.

      if wa_notifheader_export-notif_no is not initial.
        me->z_modify_nota_emprestimo(
          exporting
            equipment = equipment
            planplant = planplant
            tplnr     = funct_loc
            notif_no  = wa_notifheader_export-notif_no
        ).
      endif.
    endif.


*    CLEAR: local.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Criar ordem de manutanção devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_ORDER_MAINTAIN

**********************************************************************************
*& Descrição: Encerrar ordens de manutenção do equipamento                      &*
*& Atributo.: WA_DATA_GENERAL, WA_RETURN                                        &*
*& Parâmetro: EQUIPMENT                                                         &*
**********************************************************************************
  method z_encerrar_todas_ordens.

    clear: it_methods, wa_return.
    data: lv_refnum     type ifrefnum,
          vg_aufnr      type aufnr,
          ws_data_ordem type viaufkst.

*     DATA:  EQUIPMENT  TYPE EQUNR,
*             STANDORDER TYPE DAUFN,
*             SETTLORDER TYPE ILOM_ORDST.

    if standorder is not initial.
      clear: wa_methods, wa_header, wa_return.
      free: it_methods, it_header,it_return, t_return.

      wa_methods-refnumber = 1.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'TECHNICALCOMPLETE '.
      wa_methods-objectkey = standorder.
      append wa_methods to it_methods.

      wa_methods-objecttype = ''.
      wa_methods-method = 'SAVE'.
      append wa_methods to it_methods.

      wa_header-orderid = standorder.
      append wa_header to it_header.

      free: it_return.
      call function 'BAPI_ALM_ORDER_MAINTAIN'
        tables
          it_methods = it_methods
          it_header  = it_header
          return     = it_return.

      append lines of it_return to t_return.
      if not line_exists( it_return[ type = 'E' ] ).

        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait   = 'X'
          importing
            return = wa_return.
      endif.
    endif.

    if settlorder is not initial.
      clear: wa_methods, wa_header, wa_return.
      free: it_methods, it_header,it_return, t_return.

      wa_methods-refnumber = 1.
      wa_methods-objecttype = 'HEADER'.
      wa_methods-method = 'TECHNICALCOMPLETE '.
      wa_methods-objectkey = settlorder.
      append wa_methods to it_methods.

      wa_methods-objecttype = ''.
      wa_methods-method = 'SAVE'.
      append wa_methods to it_methods.

      wa_header-orderid = settlorder.
      append wa_header to it_header.

      free: it_return.
      call function 'BAPI_ALM_ORDER_MAINTAIN'
        tables
          it_methods = it_methods
          it_header  = it_header
          return     = it_return.

      append lines of it_return to t_return.
      if not line_exists( it_return[ type = 'E' ] ).
        clear: wa_return.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait   = 'X'
          importing
            return = wa_return.
        wait up to 2 seconds.
      endif.
    endif.

    "Verificar se tem nota vinculada e encerrar.
    if it_methods is not initial.
      loop at it_methods assigning field-symbol(<ws_method>).

        clear: ws_data_ordem.
        if <ws_method>-objectkey is not initial.
          vg_aufnr = <ws_method>-objectkey.
          z_get_data_ordem(
            exporting
              aufnr         = conv #( vg_aufnr )
            importing
              ws_data_ordem = ws_data_ordem
          ).

          if ws_data_ordem-qmnum is not initial.
            z_close_nota_emprestimo( notif_no = ws_data_ordem-qmnum
                                     refdate  = sy-datum
                                     reftime  = sy-uzeit ).
          endif.
        endif.
        clear:  ws_data_ordem, vg_aufnr.
      endloop.
    endif.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'encerrar ordem '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_ENCERRAR_TODAS_ORDENS

**********************************************************************************
*& Descrição: Verificar equipamento superior                                    &*
*& Atributo.: WA_EQUZ                                                           &*
*& Parâmetro: EQUIPMENT                                                         &*
**********************************************************************************
  method z_equi_superior.
    clear: wa_retorn.

    select single *
    from equz as a
    inner join equi as b on b~equnr eq a~equnr
    into corresponding fields of w_equz
    where a~equnr eq equnr
      and a~iwerk eq iwerk
      and a~datbi eq '99991231'
      and b~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A

    if w_equz-hequi is not initial.
      wa_retorn = abap_true.
    endif.
    clear w_equz.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Verificar equipamento superior'
                  filial_origem   = iwerk
                  filial_destino  = iwerk
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.


**********************************************************************************
*& Descrição: Encerrar ordens de manutenção do equipamento                      &*
*& Atributo.: WA_DATA_GENERAL, WA_RETURN                                        &*
*& Parâmetro: EQUIPMENT                                                         &*
**********************************************************************************
  method z_encerrar_todas_ordens_dev.

    clear: it_methods, wa_return.

    data: lv_refnum  type ifrefnum.

    define add_wa_methods.
      wa_methods-refnumber  = &1.
      wa_methods-objecttype = &2.
      wa_methods-method     = &3.
      wa_methods-objectkey  = &4.
      append wa_methods to it_methods.
      clear wa_methods.
    end-of-definition.

    lv_refnum  = 1.

    shift lv_refnum right deleting trailing space.
    translate lv_refnum using '0'.

    if standorder     is not initial.
      add_wa_methods:
       lv_refnum 'HEADER'  'TECHNICALCOMPLETE' standorder.
    endif.

    if settlorder is not initial.
      add_wa_methods:
       lv_refnum 'HEADER'  'TECHNICALCOMPLETE' settlorder.
    endif.

    add_wa_methods:
     lv_refnum       ''               'SAVE' '1'.

    call function 'BAPI_ALM_ORDER_MAINTAIN'
      tables
        it_methods = it_methods.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
  endmethod.                    "Z_ENCERRAR_TODAS_ORDENS

**********************************************************************************
*& Descrição: Recebe número da ordem criada                                     &*
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Retorno..: NUMERO_ORDEM                                                      &*
**********************************************************************************
  method z_criar_ordens_manutenc_get.
    numero_ordem = me->at_numero_ordem.
  endmethod.                    "Z_CRIAR_ORDENS_MANUTENC_GET


**********************************************************************************
*& Descrição: Recebe número da ordem criada                                     &*
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Retorno..: NUMERO_ORDEM                                                      &*
**********************************************************************************
  method z_criar_ordens_manutenc_get_dv.
    clear numero_ordem.
    numero_ordem = me->at_numero_ordem.
  endmethod.                    "Z_CRIAR_ORDENS_MANUTENC_GET

**********************************************************************************
*& Descrição: Exibir status do equipamento                                      &*
*& Parâmetro: EQUIPMENT, IT_SYSTEM_STATUS, IT_USER_STATUS                       &*
*& Atributos Globais                                                            &*
**********************************************************************************
  method z_status_equipamento.

    clear: wa_return, it_system_status,
           it_user_status.

    call function 'BAPI_EQUI_GETSTATUS'
      exporting
        equipment     = equipment
        language      = sy-langu
      importing
        return        = wa_return
      tables
        system_status = it_system_status
        user_status   = it_user_status.
  endmethod.                    "Z_STATUS_EQUIPAMENTO

**********************************************************************************
*& Descrição: Exibir detalhes do equipamento                                    &*
*& Parâmetro: EQUIPMENT,                                                        &*
*& Atributos: WA_DATA_GENERAL, WA_RETUR                                         &*
**********************************************************************************

  method z_detalhes_equipamento.

    clear: wa_data_general, wa_return
         , at_data_general-standorder
         , at_data_general-settlorder.

    free: it_zpmt0079.

    call function 'BAPI_EQUI_GETDETAIL'
      exporting
        equipment        = equipment
      importing
        data_general_exp = wa_data_general
        return           = wa_return.

    at_costcenter_origem       = wa_data_general-costcenter.
    at_data_general-standorder = wa_data_general-standorder.
    at_data_general-settlorder = wa_data_general-settlorder.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = equipment
                  tp_processo     = 'Recuperar dados equipamento'
                  filial_origem   = wa_data_general-costcenter
                  filial_destino  = ''
                  kostl_origem    = equipment
                  kostl_destino   = equipment
                  tcode           = sy-tcode
                  bezei           = equipment
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  endmethod.                    "Z_MODIFICAR_EQUIPAMENTO

**********************************************************************************
*& Descrição: Desinstalar equipamento do local atual                            &*
*& Parâmetro: EQUIPMENT,                                                        &*
*& Atributos: WA_RETURN_BAPI_EQMT                                               &*
**********************************************************************************
  method z_desinstal_equipamento.
    data: ws_veiculo   type ztpm_m_veic_mobile.

    clear: wa_return_bapi_eqmt, wa_return.
    data: local   type iflot,
          lv_line type bsvx-sttxt.

    select single a~mandt e~tplnr e~pltxt a~equnr a~erdat a~aedat b~eqtyp b~eqart b~objnr a~iwerk a~datbi a~hequi c~eqktx
    from equz as a
    inner join equi as b on b~equnr eq a~equnr
    inner join eqkt as c on c~equnr eq b~equnr
    inner join iloa as d on d~iloan eq a~iloan
    inner join iflotx as e on e~tplnr eq d~tplnr
    into corresponding fields of ws_veiculo
      where a~equnr eq equipment
       and  a~datbi eq '99991231'
       and  c~spras eq sy-langu.

    if ws_veiculo-tplnr is initial.
      ws_veiculo-tplnr = funcloc.
    endif.

    clear: me->zlocal_origem.
    me->zlocal_origem = ws_veiculo-tplnr.

*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
    if ws_veiculo-objnr is not initial.

      call function 'STATUS_TEXT_EDIT'
        exporting
          client           = sy-mandt
          objnr            = ws_veiculo-objnr
          spras            = sy-langu
        importing
          line             = lv_line
        exceptions
          object_not_found = 1
          others           = 2.
      if sy-subrc = 0.
        if lv_line eq 'INAT' or lv_line eq 'MREL'.
          ws_veiculo-istat = '0'.
        else.
          ws_veiculo-istat = '1'.
        endif.
      endif.

    endif.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

    call function 'BAPI_EQMT_DISMANTLEFL'
      exporting
        equipment = equipment
        funcloc   = ws_veiculo-tplnr
        date      = sy-datlo
        time      = sy-timlo
      importing
        return    = wa_return_bapi_eqmt.

*-US 158036-13-12-2024-#158036-RJF-Início
    if wa_return_bapi_eqmt-type eq 'E'.
      gw_erro = corresponding #( wa_return_bapi_eqmt ).
      call function 'BAPI_TRANSACTION_ROLLBACK'
        importing
          return = wa_return.
    else.
*-US 158036-13-12-2024-#158036-RJF-Fim

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = 'X'
        importing
          return = wa_return.
      wait up to 02 seconds.

*-US 158036-13-12-2024-#158036-RJF-Início
    endif.
*-US 158036-13-12-2024-#158036-RJF-Fim


*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Desinstalar equipamento do local atual '
                  filial_origem   = ws_veiculo-tplnr
                  filial_destino  = ws_veiculo-tplnr
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_EQUI_DISMANTLE


**********************************************************************************
*& Descrição: Seleciona local de transferencia                           &*                                                                              *
*& Atributo.: local                                                   &*
*& Parâmetro: werks                           &*
**********************************************************************************
  method z_seleciona_local_tranf.
    data: ws_iflo type iflo,
          zfltyp  type iflo-fltyp.
    "Seleciona o local de transferencia.
    clear: local, zfltyp, ws_iflo.
    zfltyp = 'Y'.
    select single *
    from iflo into ws_iflo
    where iwerk eq werks
      and fltyp eq zfltyp.
    if sy-subrc eq 0.
      local = ws_iflo-tplnr.
    endif.

  endmethod.                    "Z_LIMPAR_TELA

**********************************************************************************
*& Descrição: Instalar equipamento no centro destino                            &*                                                                              *
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Parâmetro: EQUIPMENT, FUNCLOC, WA_RETURN_BAPI_EQMT                           &*
**********************************************************************************
  method z_instalar_equipamento.

    clear: wa_return_bapi_eqmt, wa_return.

    call function 'BAPI_EQMT_INSTALLFL'
      exporting
        equipment = equipment
        funcloc   = tplnr
        date      = sy-datlo
        time      = sy-timlo
      importing
        return    = wa_return_bapi_eqmt.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
    wait up to 2 seconds.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Instalar equipamento '
                  filial_origem   = tplnr
                  filial_destino  = tplnr
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_EQUI_INSTALLFL

**********************************************************************************
*& Descrição: Instalar equipamento no centro destino                            &*                                                                              *
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Parâmetro: EQUIPMENT, FUNCLOC, WA_RETURN_BAPI_EQMT                           &*
**********************************************************************************
  method z_instalar_equipamento_dev.
    clear: wa_return_bapi_eqmt.


    call function 'BAPI_EQMT_INSTALLFL'
      exporting
        equipment = equipment
        funcloc   = tplnr
        date      = sy-datlo
        time      = sy-timlo
      importing
        return    = wa_return_bapi_eqmt.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
    wait up to 2 seconds.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Instala equipamento devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_EQUI_INSTALLFL

**********************************************************************************
*& Descrição: Modificar equipamento com centro destino                          &*
*& Atributo.: WA_DATA_GENERAL, WA_DATA_GENERALX, WA_RETURN,                     &*
*&            WA_DATA_SPECIFIC, WA_DATA_SPECIFICX                               &*
*& Parâmetro: MAINTPLANT, PLANPLANT, BUS_AREA, COSTCENTER, WORK_CTR,            &*
*&            STANDORDER, SETTLORDER                                            &*
**********************************************************************************
  method z_modificar_equipamento.

    clear: wa_data_general, wa_data_generalx, wa_data_specific,
           wa_data_specificx, wa_return.
    data: lv_empresa type bukrs.

    "Buscar empresa

    select single bukrs
        into lv_empresa
        from j_1bbranch
        where branch eq planplant.

    "FF - 10/04/2024 #137726 - inicio
    wa_data_general-asset_no   = |{ imobilizado alpha = in }|.
    wa_data_general-sub_number = subimobilizado.
    wa_data_general-plangroup  = 'FRO'.
    wa_data_general-authgrp    = '0001'. "De 0006 para 0001
    "FF - 10/04/2024 #137726 - fim

    wa_data_general-maintplant = maintplant.
    wa_data_general-bus_area   = bus_area  .
    wa_data_general-costcenter = costcenter.
    wa_data_general-comp_code  = lv_empresa.
    wa_data_general-planplant  = planplant.
    wa_data_general-work_ctr   = work_ctr.
    wa_data_general-standorder = standorder.
    wa_data_general-settlorder = settlorder.

    "FF - 10/04/2024 #137726 - inicio
    if wa_data_general-asset_no is not initial.
      wa_data_generalx-asset_no   = 'X'.
    endif.

    if wa_data_general-sub_number is not initial.
      wa_data_generalx-sub_number = 'X'.
    endif.

    wa_data_generalx-plangroup  = 'X'.
    wa_data_generalx-authgrp    = 'X'.
    "FF - 10/04/2024 #137726 - fim

    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-maintplant = 'X'.
    wa_data_generalx-bus_area   = 'X'.
    wa_data_generalx-costcenter = 'X'.
    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-work_ctr   = 'X'.

    if wa_data_general-standorder is not initial.
      wa_data_generalx-standorder = 'X'.
    endif.

    if wa_data_general-settlorder is not initial.
      wa_data_generalx-settlorder = 'X'.
    endif.

    call function 'BAPI_EQUI_CHANGE'
      exporting
        equipment      = equipment
        data_general   = wa_data_general
        data_generalx  = wa_data_generalx
        data_specific  = wa_data_specific
        data_specificx = wa_data_specificx
        valid_date     = sy-datum
        valid_time     = sy-uzeit
      importing
        return         = wa_return.
*        RETURN         = IT_RETURN.

*-US 158036-13-12-2024-#158036-RJF-Inicio
    if wa_return-type eq 'E'.
      gw_erro = wa_return.
      call function 'BAPI_TRANSACTION_ROLLBACK'
        importing
          return = wa_return.
    else.
*-US 158036-13-12-2024-#158036-RJF-Fim

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = 'X'
        importing
          return = wa_return.
      wait up to 02 seconds.

*-US 158036-13-12-2024-#158036-RJF-Inicio
    endif.
*-US 158036-13-12-2024-#158036-RJF-Fim

*    CALL FUNCTION 'ENQUE_SLEEP'
*      EXPORTING
*        SECONDS = 10.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Modificar equipamento '
                  filial_origem   = planplant
                  filial_destino  = planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = costcenter
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_EQUI_CHANGE


**********************************************************************************
*& Descrição: Modificar equipamento com centro destino                          &*
*& Atributo.: WA_DATA_GENERAL, WA_DATA_GENERALX, WA_RETURN,                     &*
*&            WA_DATA_SPECIFIC, WA_DATA_SPECIFICX                               &*
*& Parâmetro: MAINTPLANT, PLANPLANT, BUS_AREA, COSTCENTER, WORK_CTR,            &*
*&            STANDORDER, SETTLORDER                                            &*
**********************************************************************************
  method z_modificar_equipamento_dev.
    data: lv_empresa type bukrs.

    clear: wa_data_general, wa_data_generalx, wa_data_specific,
           wa_data_specificx, wa_return,lv_empresa.

    select single bukrs
         into lv_empresa
         from j_1bbranch
         where branch eq planplant.

    "FF - 10/04/2024 #137726 - inicio
    wa_data_general-asset_no   = imobilizado.
    wa_data_general-sub_number = subimobilizado.
    wa_data_general-plangroup  = 'FRO'.
    wa_data_general-authgrp    = '0006'. "De 0001 para 0006
    "FF - 10/04/2024 #137726 - fim

    wa_data_general-maintplant = maintplant.
    wa_data_general-bus_area   = bus_area  .
    wa_data_general-costcenter = costcenter.
    wa_data_general-comp_code  = lv_empresa.
    wa_data_general-planplant  = planplant.
    wa_data_general-work_ctr   = work_ctr.

    wa_data_general-standorder = standorder.
    wa_data_general-settlorder = settlorder.

    "FF - 10/04/2024 #137726 - inicio
    if wa_data_general-asset_no is not initial.
      wa_data_generalx-asset_no   = 'X'.
    endif.

    if wa_data_general-sub_number is not initial.
      wa_data_generalx-sub_number = 'X'.
    endif.

    wa_data_generalx-plangroup  = 'X'.
    wa_data_generalx-authgrp    = 'X'.
    "FF - 10/04/2024 #137726 - fim

    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-maintplant = 'X'.
    wa_data_generalx-bus_area   = 'X'.
    wa_data_generalx-costcenter = 'X'.
    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-work_ctr   = 'X'.

    if wa_data_general-standorder is not initial.
      wa_data_generalx-standorder = 'X'.
    endif.

    if wa_data_general-settlorder is not initial.
      wa_data_generalx-settlorder = 'X'.
    endif.

*    WA_DATA_SPECIFIC-READ_FLOC  = |{ PLANPLANT }.{ 'FRO' }|.

    call function 'BAPI_EQUI_CHANGE'
      exporting
        equipment      = equipment
        data_general   = wa_data_general
        data_generalx  = wa_data_generalx
        data_specific  = wa_data_specific
        data_specificx = wa_data_specificx
        valid_date     = sy-datum
        valid_time     = sy-uzeit
      importing
        return         = wa_return.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.
    wait up to 2 seconds.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Modifica equipamento devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                    "Z_EQUI_CHANGE

**********************************************************************************
*& Descrição: Selecionar os planos referêntes ao equipamento                    &*                                                                 &*
*& Parâmetro: EQUIPMENT                                                         &*
*& Atributos Globais                                                            &*
**********************************************************************************
  method z_selecionar_planos.

    data: it_equnr type range of equi-equnr,
          it_warpl type range of vimpla-warpl,
          it_mptyp type range of vimpla-mptyp,
          it_strat type range of vimpla-strat,
          it_tplnr type range of vimpla-tplnr,
          it_kdauf type range of vimpla-kdauf,
          it_kdpos type range of vimpla-kdpos,
          it_prot  type range of sprot_u,
          wa_equnr like line of it_equnr.

    clear: it_equnr,it_warpl,it_mptyp,it_strat,it_tplnr,
           it_kdauf,it_kdpos,it_prot.

*   Função "MAINTENANCE_PLAN_SELECTION" retorna os planos do eqpto.

    wa_equnr-low    = equipment.
    wa_equnr-sign   = 'I' .
    wa_equnr-option = 'EQ'.
    append wa_equnr to it_equnr.
    clear wa_equnr.

    call function 'MAINTENANCE_PLAN_SELECTION'
      tables
        i_warpl = it_warpl
        i_mptyp = it_mptyp
        i_strat = it_strat
        i_equnr = it_equnr
        i_tplnr = it_tplnr
        i_kdauf = it_kdauf
        i_kdpos = it_kdpos
        i_selc  = it_selc_plan
        i_prot  = it_prot.

  endmethod.                    "Z_PLAN_SELECTION

**********************************************************************************
*& Descrição: Selecionar os planos referêntes ao equipamento                    &*                                                                 &*
*& Parâmetro: EQUIPMENT                                                         &*
*& Atributos Globais                                                            &*
**********************************************************************************
  method z_selecionar_planos_dev.

    data: it_equnr type range of equi-equnr,
          it_warpl type range of vimpla-warpl,
          it_mptyp type range of vimpla-mptyp,
          it_strat type range of vimpla-strat,
          it_tplnr type range of vimpla-tplnr,
          it_kdauf type range of vimpla-kdauf,
          it_kdpos type range of vimpla-kdpos,
          it_prot  type range of sprot_u,
          wa_equnr like line of it_equnr.

    clear: it_equnr,it_warpl,it_mptyp,it_strat,it_tplnr,
           it_kdauf,it_kdpos,it_prot.

*   Função "MAINTENANCE_PLAN_SELECTION" retorna os planos do eqpto.

    wa_equnr-low    = equipment.
    wa_equnr-sign   = 'I' .
    wa_equnr-option = 'EQ'.
    append wa_equnr to it_equnr.
    clear wa_equnr.

    call function 'MAINTENANCE_PLAN_SELECTION'
      tables
        i_warpl = it_warpl
        i_mptyp = it_mptyp
        i_strat = it_strat
        i_equnr = it_equnr
        i_tplnr = it_tplnr
        i_kdauf = it_kdauf
        i_kdpos = it_kdpos
        i_selc  = it_selc_plan
        i_prot  = it_prot.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Selecionar planos devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


    if it_zpmt0079 is not initial.
      modify zpmt0079 from table it_zpmt0079.
      commit work.
    endif.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim

  endmethod.                    "Z_PLAN_SELECTION

**********************************************************************************
*& Descrição: Capturar ID do equipamento. Ex: (OFICINA)                         &*
*& Parâmetro: WA_MPOS - Declarado no programa                                   &*
*& Atributos Globais                                                            &*
**********************************************************************************
  method z_set_id_equipament.
    clear wa_mpos.

    select single *
      from mpos
      into wa_mpos
     where iwerk = swerk
       and equnr = equnr
       and gewrk <> ''.

    id_equipment = wa_mpos-gewrk.

  endmethod.                    "Z_SET_ID_EQUIPAMENT

*& Descrição: Capturar ID do equipamento. Ex: (OFICINA)                         &*
*& Parâmetro: WA_MPOS - Declarado no programa                                   &*
*& Atributos Globais                                                            &*
**********************************************************************************
  method z_set_id_equipament_dev.
    clear wa_mpos.

    select single *
      from mpos
      into wa_mpos
     where iwerk = swerk
       and gewrk <> ''.

    id_equipment = wa_mpos-gewrk.

  endmethod.                    "Z_SET_ID_EQUIPAMENT

**********************************************************************************
*& Descrição: Modificar todos os planos referente ao equipamento                &*
*& Atributo.: WA_IMPOS, IT_IMHIS, IT_IMPLA, IT_IMMPT, - Decl. no programa       &*
*&            WA_RETURN                                                         &*
*& Parâmetro: EQUIPMENT, IWERK, GEWRK, GSBER, LAUFN                             &*                                         *
**********************************************************************************
  method z_modificar_planos.

    clear: it_impos, it_imhis,
           it_immpt, it_impla,
           it_impos[], it_imhis[],
           it_immpt[], it_impla[].

*     Percorre todos os planos do equipamento

    loop at it_selc_plan into wa_selc_plan.

*     Capturar as informações do plano do equipamento.

      select single *
        from mpos
        into wa_impos
       where warpl = wa_selc_plan-warpl.

*     Capturar o numerador de grupos referente ao centro destino.

      select single *
        from plko
        into wa_plko
       where plnnr = wa_impos-plnnr
         and werks = tbx_centro_destino
         and plnty = 'A'.

*      z_seleciona_local_tranf(
*        EXPORTING
*          werks = swerk
*        IMPORTING
*          local = DATA(_tplnr)
*      ).

*      DATA(_tplnr) = |{ swerk }.{ 'FRO' }|.
*      IF _tplnr IS NOT INITIAL.
      select single *
      from iloa
      into @data(_iloa)
        where tplnr eq @tplnr.
*      ENDIF.

      if  _iloa is not initial.
        data(_iloan) = _iloa-iloan.
      endif.

*     Modifica o plano conforme informações do destino.

      wa_impos-equnr = equipment.
      wa_impos-aedat = sy-datum.
      wa_impos-aenam = sy-uname.
      wa_impos-plnty = wa_plko-plnty.
      wa_impos-plnnr = wa_plko-plnnr.
      wa_impos-plnal = wa_plko-plnal.
      wa_impos-iwerk = swerk.
      wa_impos-gewrk = gewrk.
      wa_impos-gsber = gsber.
      wa_impos-iloan = _iloan.
      wa_impos-tplnr = loc_instalacao. "FF - 22.11.2023 - ins
*      WA_IMPOS-LAUFN = LAUFN.
      append wa_impos to it_impos.

*FF - 22.11.2023 - inicio
      update iloa set swerk = swerk
                      gsber = gsber
                      kostl = at_costcenter_destino
                      where iloan = wa_impos-iloan.

      commit work.
*FF - 22.11.2023 - fim


    endloop.

    call function 'MAINTENANCE_PLAN_POST'
      exporting
        x_xaktyp = 'V'
      tables
        imhis    = it_imhis
        immpt    = it_immpt
        impla    = it_impla
        impos    = it_impos.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = wa_return.

    wait up to 02 seconds.

    clear: _iloan,  _iloa.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Modifica plano '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Emprestimo'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.

*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                "Z_MODIFICAR_PLANOS



**********************************************************************************
*& Descrição: Modificar todos os planos referente ao equipamento                &*
*& Atributo.: WA_IMPOS, IT_IMHIS, IT_IMPLA, IT_IMMPT, - Decl. no programa       &*
*&            WA_RETURN                                                         &*
*& Parâmetro: EQUIPMENT, IWERK, GEWRK, GSBER, LAUFN                             &*                                         *
**********************************************************************************
  method z_modificar_planos_dev.

    clear: it_impos, it_imhis,
           it_immpt, it_impla.

*     Percorre todos os planos do equipamento

    loop at it_selc_plan into wa_selc_plan.

*     Capturar as informações do plano do equipamento.

      select single *
        from mpos
        into wa_impos
       where warpl = wa_selc_plan-warpl.

*     Capturar o numerador de grupos referente ao centro destino.

      select single *
        from plko
        into wa_plko
       where plnnr eq wa_impos-bautl
         and werks = swerk
         and plnty = 'A'.

      z_seleciona_local_tranf(
        exporting
          werks = swerk
        importing
          local = data(_tplnr)
      ).

      if _tplnr is initial.
        concatenate swerk '.FRO' into _tplnr.
      endif.

      if _tplnr is not initial.
        select single *
        from iloa
        into @data(_iloa)
          where tplnr eq @_tplnr.
      endif.

      if  _iloa is not initial.
        data(_iloan) = _iloa-iloan.
      endif.

*     Modifica o plano conforme informações do destino.

      wa_impos-equnr = equipment.
      wa_impos-aedat = sy-datum.
      wa_impos-aenam = sy-uname.
      wa_impos-plnty = wa_plko-plnty.
      wa_impos-plnnr = wa_plko-plnnr.
      wa_impos-plnal = wa_plko-plnal.
      wa_impos-iwerk = swerk.
      wa_impos-gewrk = gewrk.
      wa_impos-gsber = gsber.
      wa_impos-iloan = _iloan.
      wa_impos-tplnr = loc_instalacao. "FF - 22.11.2023 - ins
*      WA_IMPOS-LAUFN = LAUFN.
      append wa_impos to it_impos.

*FF - 22.11.2023 - inicio
      update iloa set swerk = swerk
                      gsber = gsber
                      kostl = at_costcenter_destino
                      where iloan = wa_impos-iloan.

      commit work.
*FF - 22.11.2023 - fim

    endloop.

    call function 'MAINTENANCE_PLAN_POST'
      exporting
        x_xaktyp = 'V'
      tables
        imhis    = it_imhis
        immpt    = it_immpt
        impla    = it_impla
        impos    = it_impos.

    call function 'BAPI_TRANSACTION_COMMIT'
      importing
        return = wa_return.
    wait up to 2 seconds.

    clear: _tplnr, _iloan,  _iloa.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'modificar planos devolução '
                  filial_origem   = wa_data_general-planplant
                  filial_destino  = wa_data_general-planplant
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = 'Devolução'
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


    if it_zpmt0079 is not initial.
      modify zpmt0079 from table it_zpmt0079.
      commit work.
    endif.
*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.                "Z_MODIFICAR_PLANOS

  method z_busca_imobilizado.
**********************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
**********************************************************************************************

*-US 142094-08-10-2024-#142094-RJF-Inicio
*    SELECT SINGLE EQUNR,  ANLNR, ANLUN
*    INTO @DATA(LS_ITOB)
*    FROM ITOB
*    WHERE EQUNR = @EQUIPAMENTO
*      AND IWERK = @IWERK.
*
*    IF SY-SUBRC IS INITIAL.
*      IMOBILIZADO = LS_ITOB-ANLNR.
*    ELSE.
*-US 142094-08-10-2024-#142094-RJF-fim
    clear imobilizado .
    select single a~*
      from fleet as a
      inner join equi as b on b~objnr eq a~objnr
      into  @data(wa_fleet)
      where b~equnr =  @equipamento.

    imobilizado = wa_fleet-zzimobilizado.
    "SUBIMOBILIZADO = "NÃO VAI TER
*-US 142094-08-10-2024-#142094-RJF-Inicio
*    ENDIF.
*-US 142094-08-10-2024-#142094-RJF-fim
**********************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - FIM
**********************************************************************************************
*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
    append value #( equnr = tbx_equipamento
                  tp_processo     = 'Busca imobilizado '
                  filial_origem   = iwerk
                  filial_destino  = iwerk
                  kostl_origem    = wa_data_general-costcenter
                  kostl_destino   = at_costcenter_destino
                  tcode           = sy-tcode
                  bezei           = tbx_equipamento
                  us_criacao      = sy-uname
                  dt_criacao      = sy-datum
                  hr_criacao      = sy-uzeit ) to it_zpmt0079.


*GGARAUJO1 - 12/09/2024 - IR190024 - fim
  endmethod.

endclass.                    "ZBAPIS IMPLEMENTATION


*&-----------------------------------------------------------------------------*
*& CLASS LCL_EVENT_DEFINITION                                                  *
*& AUTOR: ENIO JESUS                                                           *
*& 13.07.2015                                                                  *
*&-----------------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.

    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    class-methods:
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column.

    class-methods:
      set_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object.

    class-methods:
      get_ucomm for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "LCL_EVENT_HANDLER DEFINITION

*&-------------------------------------------------------------------*
*&  CLASS LCL_EVENT_HANDLER IMPLEMENTATION                           *
*&  AUTOR: ENIO JESUS                                                *
*&  13.07.2015                                                       *
*&-------------------------------------------------------------------*
class lcl_event_handler implementation.
  method on_double_click.
    check e_row-rowtype is initial.

    case g_ts_0100-subscreen.
      when '0110'.
        perform sel_dados_equip using e_row e_column-fieldname.

      when '0120'.
        perform sel_dados_equip_emprest using e_row e_column-fieldname.

      when '0130'.
        perform sel_dados_equip_resposav using e_row e_column-fieldname.
      when others.
    endcase.
  endmethod.                    "ON_DOUBLE_CLICK

**********************************************************************
*& Descrição: Registrar evento de click no checkbox                 &*
*& Parâmetro: ER_DATA_CHANGED->                                     &*
*& Atributos Globais                                                &*
********************************************************************&*
  method on_data_changed.

    data: lv_lines type sy-tabix.

    loop at er_data_changed->mt_good_cells into ls_good.

      case ls_good-fieldname.

        when 'CBX_ORD_ABAST'.
          read table it_saida_emprestimo_equi into wa_saida_emprestimo_equi
               index ls_good-row_id.

          wa_saida_emprestimo_equi-cbx_ord_abast = ls_good-value.
          modify it_saida_emprestimo_equi from wa_saida_emprestimo_equi index ls_good-row_id.

        when 'CBX_DEVOLVER'.
          read table it_saida_equi_responsavel into wa_saida_equi_responsavel
               index ls_good-row_id.

          wa_saida_equi_responsavel-cbx_devolver = ls_good-value.
          modify it_saida_equi_responsavel from wa_saida_equi_responsavel index ls_good-row_id.

        when 'DT_DEVOLUCAO'.
          read table it_saida_equi_responsavel into wa_saida_equi_responsavel
               index ls_good-row_id.

          wa_saida_equi_responsavel-dt_devolucao = ls_good-value.
          modify it_saida_equi_responsavel from wa_saida_equi_responsavel index ls_good-row_id.

          call method obj_alv_0130->refresh_table_display
            exporting
              is_stable = wa_stable.

        when 'HR_DEVOLUCAO'.
          read table it_saida_equi_responsavel into wa_saida_equi_responsavel
               index ls_good-row_id.

          wa_saida_equi_responsavel-hr_devolucao = ls_good-value.
          modify it_saida_equi_responsavel from wa_saida_equi_responsavel index ls_good-row_id.

          call method obj_alv_0130->refresh_table_display
            exporting
              is_stable = wa_stable.
*** Inicio - Rubenilson - 24.12.24 - US138088
        when 'DEVOLUCAO_AUTOMATICA'.
          read table it_saida_emprestimo_equi into wa_saida_emprestimo_equi
               index ls_good-row_id.

          loop at it_saida_emprestimo_equi assigning field-symbol(<fs_emp_equi>).
            <fs_emp_equi>-devolucao_automatica = ls_good-value.
          endloop.

          wa_saida_emprestimo_equi-devolucao_automatica = ls_good-value.
          modify it_saida_emprestimo_equi from wa_saida_emprestimo_equi index ls_good-row_id.

          call method obj_alv_0200->refresh_table_display
            exporting
              is_stable = wa_stable.

*** Fim - Rubenilson - 24.12.24 - US138088
      endcase.
    endloop.

    clear: wa_saida_equi_responsavel, wa_saida_equi_disponiveis, wa_saida_equi_emprestados,
           ls_good, er_data_changed->mt_good_cells.
  endmethod.                    "ON_DATA_CHANGED_CHECKBOX

  method on_data_changed_finished.

    call function 'Z_DOC_CHECK_NEW'
      exporting
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
      importing
        e_messagem = wa_mensagem
      tables
        it_msgs    = it_msg_return.

  endmethod.                    "ON_DATA_CHANGED_FINISHED

**********************************************************************
*& Descrição: Criar botões em ALV                                   &*
*& Parâmetro: E_OBJECT->                                            &*
*& Atributos Globais                                                &*
********************************************************************&*
  method set_toolbar.

    clear: wa_toolbar, e_object->mt_toolbar.
    case g_ts_0100-pressed_tab.

*    Botão <Emprestar equipamentos> Tela 0100;
      when 'TAB_DISPONIVEIS'.
        wa_toolbar-butn_type = 3.
        append wa_toolbar to e_object->mt_toolbar.

        clear wa_toolbar.
        wa_toolbar-function     = 'BTN_EMPRESTAR_EQUI'.
        wa_toolbar-icon         =  icon_delivery.
        wa_toolbar-quickinfo    = 'Emprestar Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Emprestar Equipamentos'.
        append wa_toolbar to e_object->mt_toolbar.

        clear wa_toolbar.
        wa_toolbar-function     = '&MB_FILTER'.
        wa_toolbar-icon         =  icon_filter.
        wa_toolbar-quickinfo    = 'Filtrar Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Filtrar Equipamentos'.
        append wa_toolbar to e_object->mt_toolbar.

      when 'TAB_RESPONSAVEL'.
        wa_toolbar-butn_type = 3.
        append wa_toolbar to e_object->mt_toolbar.
        clear wa_toolbar.

        wa_toolbar-function     = 'BTN_DEVOLVER_EQUI'.
        wa_toolbar-icon         =  icon_transportation_mode.
        wa_toolbar-quickinfo    = 'Devolver Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Devolver Equipamentos'.
        append wa_toolbar to e_object->mt_toolbar.

        clear wa_toolbar.
        wa_toolbar-function     = '&MB_FILTER'.
        wa_toolbar-icon         =  icon_filter.
        wa_toolbar-quickinfo    = 'Filtrar Equipamentos'.
        wa_toolbar-butn_type    = 0.
        wa_toolbar-text         = 'Filtrar Equipamentos'.
        append wa_toolbar to e_object->mt_toolbar.
      when others.
    endcase.

    if ( sy-dynnr = '0300' ).
      clear: wa_toolbar, e_object->mt_toolbar.

      wa_toolbar-function     = 'BTN_SELECIONAR'.
      wa_toolbar-icon         = icon_system_mark.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Selecionar'.
      append wa_toolbar to e_object->mt_toolbar.

      wa_toolbar-butn_type = 3.
      append wa_toolbar to e_object->mt_toolbar.
      clear wa_toolbar.

      wa_toolbar-function     = 'BTN_DESMONTAR'.
      wa_toolbar-icon         = icon_convert_all.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Desmontar'.
      append wa_toolbar to e_object->mt_toolbar.

      wa_toolbar-butn_type = 3.
      append wa_toolbar to e_object->mt_toolbar.
      clear wa_toolbar.

      wa_toolbar-function     = 'BTN_ABOUT'.
      wa_toolbar-icon         = icon_information.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Sobre'.
      append wa_toolbar to e_object->mt_toolbar.

    endif.

  endmethod.                    "SET_TOOLBAR

**********************************************************************
*& Descrição: Registra ação nos botões da ALV                       &*
*& Parâmetro: E_UCOMM                                               &*
*& Atributos Globais                                                &*
********************************************************************&*
  method get_ucomm.

    data: r_checar_info_emprestimo  type ref to zuteis,
          r_checar_info_devolucao   type ref to zuteis,
          r_iniciar_processo_zbapis type ref to zbapis.

    create object: r_checar_info_emprestimo,
                   r_iniciar_processo_zbapis.

    case e_ucomm.

*    Registra ação no botão 'Emprestar Equipamentos' Tela 0100.
*    RETURN_STATUS é um atributo global que retorna se a operação não foi bem sucedida.

      when 'BTN_FILT_EQUI'.

        data: p_tname type char50.
        data: p_fname type char50.
        data: dfies_tab type table of equi.

*        PERFORM FILT.

      when 'BTN_EMPRESTAR_EQUI'.
        clear: it_selected_rows, it_saida_emprestimo_equi, return_status, wa_saida_emprestimo_equi, wa_saida_equi_disponiveis.

*        IF IT_MSG_RETURN IS NOT INITIAL.
*          MESSAGE 'Corrigir erros pendentes' TYPE 'I' DISPLAY LIKE 'E'.
*
*        ENDIF.


        call method obj_alv_0110->get_selected_rows
          importing
            et_index_rows = it_selected_rows.

        describe table it_selected_rows lines lines.

        if ( lines is initial ).
          message text-e01 type 'I' display like 'E'.

        else.
*** Inicio - Rubenilson - 23.12.24 - US138088
          data: lt_celltab type lvc_t_styl,
                wa_celltab type lvc_s_styl.

          wa_celltab-fieldname = 'CBX_ORD_ABAST'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          insert wa_celltab into lt_celltab index 1.

          wa_celltab-fieldname = 'CBX_ORD_REMON'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          insert wa_celltab into lt_celltab index 2.

          wa_celltab-fieldname = 'DEVOLUCAO_AUTOMATICA'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_enabled.

          insert wa_celltab into lt_celltab index 3.

          wa_celltab-fieldname = 'EQKTX'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          insert wa_celltab into lt_celltab index 4.

          wa_celltab-fieldname = 'EQUNR'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          insert wa_celltab into lt_celltab index 5.

          wa_celltab-fieldname = 'IWERK'.
          wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

          insert wa_celltab into lt_celltab index 6.
*** Fim - Rubenilson - 23.12.24 - US138088

          loop at it_selected_rows into wa_selected_rows.
            read table it_saida_equi_disponiveis into wa_saida_equi_disponiveis index wa_selected_rows-index.

            wa_saida_emprestimo_equi-equnr         = |{ wa_saida_equi_disponiveis-equnr alpha = out }|.
            wa_saida_emprestimo_equi-iwerk         = wa_saida_equi_disponiveis-iwerk.
            wa_saida_emprestimo_equi-eqktx         = wa_saida_equi_disponiveis-eqktx.
            wa_saida_emprestimo_equi-cbx_ord_remon = 'X'.

            clear p_eq_sup.
            if wa_saida_equi_disponiveis-cbx_ord_abast eq abap_true.
              wa_saida_emprestimo_equi-cbx_ord_abast = 'X'.
              wa_saida_emprestimo_equi-eq_sup = wa_saida_equi_disponiveis-eq_sup.
            else.
              wa_saida_emprestimo_equi-eq_inf = wa_saida_equi_disponiveis-eq_inf.
            endif.

            wa_saida_emprestimo_equi-celltab = lt_celltab."Rubenilson - 23.12.24 - US138088

            append wa_saida_emprestimo_equi to it_saida_emprestimo_equi.
            clear: wa_saida_emprestimo_equi, wa_saida_emprestimo_equi.
          endloop.

          r_checar_info_emprestimo->z_checar_equi_hierarchy( importing e_return = return_status ).

          if ( it_status_equnr is initial ).
            call screen 0200 starting at 5 5.
          else.
            call screen 0300 starting at 8 8.
          endif.
        endif.

      when 'BTN_DEVOLVER_EQUI'.

        refresh: it_selected_rows.
        data: lt_equz type equz.
        data: gt_equz      type table of equz.
        free: it_saida_dev_equi, it_status_equnr, it_selected_rows, gt_equz, it_status_hequi.

        clear: wa_selected_rows, wa_saida_equi_responsavel,
        wa_zequi_emprestimo, wa_saida_dev_equi, wa_status_equnr, wa_status_hequi..


        call method obj_alv_0130->get_selected_rows
          importing
            et_index_rows = it_selected_rows.

        describe table it_selected_rows lines lines.

        if ( lines is initial ).
          message text-e01 type 'I' display like 'E'.
          exit.
        else.
          loop at it_selected_rows into wa_selected_rows.
            read table it_saida_equi_responsavel into wa_saida_equi_responsavel index wa_selected_rows-index.
            clear wa_zequi_emprestimo.
            wa_saida_equi_responsavel-equnr = |{ wa_saida_equi_responsavel-equnr alpha = in }|.
            select single *
              from zequi_emprestimo
              into wa_zequi_emprestimo
             where equnr = wa_saida_equi_responsavel-equnr.

            if wa_zequi_emprestimo is not initial.

              move-corresponding wa_zequi_emprestimo to wa_saida_dev_equi.

              append wa_saida_dev_equi to it_saida_dev_equi.
              clear: wa_saida_dev_equi, wa_saida_equi_responsavel.
            endif.

*            IF WA_ZEQUI_EMPRESTIMO-EQUNR IS NOT INITIAL.
            free gt_equz.
            clear lt_equz.

            select single *
            from equz as a
            inner join equi as b on b~equnr eq a~equnr
            into corresponding fields of lt_equz
              where a~equnr eq wa_zequi_emprestimo-equnr
                and a~datbi eq '99991231'.
*                  AND B~EQTYP EQ 'V'.

            if lt_equz-hequi is not initial.
              read table it_saida_equi_responsavel into data(eq_respos) with key equnr =  |{ lt_equz-hequi alpha = out }|.
              if sy-subrc = 0.
                wa_status_equnr-equnr   = lt_equz-equnr."Equipamento inferior

                select single *
                from eqkt
                into @data(s_eqkt_)
                where equnr eq @wa_status_equnr-equnr.
                wa_status_equnr-eqktx = s_eqkt_-eqktx.
                wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior

                clear s_eqkt_.
                select single *
                from eqkt
                into s_eqkt_
                where equnr eq wa_status_equnr-hequi.
                wa_status_equnr-eqktx_ = s_eqkt_-eqktx.
                wa_status_equnr-eqp_sup = abap_true.
                append wa_status_equnr to it_status_equnr.
                clear wa_status_equnr.
              endif.

              free gt_equz.
              select *
              from equz as a
              inner join equi as b on b~equnr eq a~equnr
              into corresponding fields of table gt_equz
                where a~hequi eq lt_equz-equnr
                  and a~datbi eq '99991231'
                  and b~eqtyp ne 'A' "FF - 05.04.2024 - ins
                  and b~eqtyp ne 'V'
                  and b~eqtyp ne '1' "FF - 22.11.2023 - ins
                  and b~eqtyp ne '2'
                  and b~eqtyp ne '3'
                  and b~eqtyp ne '4'.


              if gt_equz is not initial.
                loop at gt_equz into data(lg_equz) where hequi eq lt_equz-equnr.
                  if sy-subrc = 0.
                    wa_status_equnr-equnr   = |{ lg_equz-equnr alpha = in }|. "Equipamento inferior
                    clear s_eqkt_.
                    select single *
                    from eqkt
                    into s_eqkt_
                      where equnr eq lg_equz-equnr.
                    wa_status_equnr-eqktx = s_eqkt_-eqktx.

                    wa_status_equnr-hequi   = lg_equz-hequi."Equipamento Superior
                    clear s_eqkt_.
                    select single *
                    from eqkt
                    into s_eqkt_
                      where equnr eq  wa_status_equnr-hequi.
                    wa_status_equnr-eqktx_ = s_eqkt_-eqktx.
                    wa_status_equnr-eqp_inf = abap_true.

                    append wa_status_equnr to it_status_equnr.
                    clear: wa_status_equnr, lg_equz.
                    continue.
                  endif.
                endloop.
              endif.

              free gt_equz.
              select *
              from equz as a
              inner join equi as b on b~equnr eq a~equnr
              into corresponding fields of table gt_equz
                where a~hequi eq lt_equz-hequi
                  and a~datbi eq '99991231'
                  and b~eqtyp ne 'A' "FF - 05.04.2024 - ins
                  and b~eqtyp ne 'V'
                  and b~eqtyp ne '1' "FF - 22.11.2023 - ins
                  and b~eqtyp ne '2'
                  and b~eqtyp ne '3'
                  and b~eqtyp ne '4'.


              if gt_equz is not initial.
                loop at gt_equz into lt_equz where hequi eq lt_equz-hequi.
                  if sy-subrc = 0.
                    wa_status_equnr-equnr   = |{ lt_equz-equnr alpha = in }|. "Equipamento inferior
                    clear s_eqkt_.
                    select single *
                    from eqkt
                    into s_eqkt_
                      where equnr eq lt_equz-equnr.
                    wa_status_equnr-eqktx = s_eqkt_-eqktx.

                    wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior
                    clear s_eqkt_.
                    select single *
                    from eqkt
                    into s_eqkt_
                      where equnr eq  wa_status_equnr-hequi.
                    wa_status_equnr-eqktx_ = s_eqkt_-eqktx.
                    wa_status_equnr-eqp_inf = abap_true.

                    append wa_status_equnr to it_status_equnr.
                    clear: wa_status_equnr, lt_equz.
                    continue.
                  endif.
                endloop.
              endif.

            else.

              free gt_equz.
              select *
              from equz as a
              inner join equi as b on b~equnr eq a~equnr
              into corresponding fields of table gt_equz
                where a~hequi eq wa_zequi_emprestimo-equnr
                  and a~datbi eq '99991231'.
*                  AND B~EQTYP EQ 'V'.

              if gt_equz is not initial.
                loop at gt_equz into lt_equz where hequi eq wa_zequi_emprestimo-equnr.
                  read table it_saida_equi_responsavel into data(_eq_respos) with key equnr =  |{ lt_equz-equnr alpha = out }|.
                  if sy-subrc = 0.
                    wa_status_equnr-equnr   = |{ _eq_respos-equnr alpha = in }|. "Equipamento inferior
                    select single *
                    from eqkt
                    into @data(s_eqkt)
                      where equnr eq  @wa_status_equnr-equnr.
                    wa_status_equnr-eqktx = s_eqkt-eqktx.

                    wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior
                    clear s_eqkt.
                    select single *
                    from eqkt
                    into s_eqkt
                      where equnr eq  wa_status_equnr-hequi.
                    wa_status_equnr-eqktx_ = s_eqkt-eqktx.
                    wa_status_equnr-eqp_inf = abap_true.

                    append wa_status_equnr to it_status_equnr.
                    clear wa_status_equnr.
                    continue.
                  endif.

                  wa_status_equnr-equnr   = |{ lt_equz-equnr alpha = in }|. "Equipamento inferior
                  clear s_eqkt.
                  select single *
                  from eqkt
                  into s_eqkt
                    where equnr eq wa_status_equnr-equnr.
                  wa_status_equnr-eqktx = s_eqkt-eqktx.
                  wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior

                  clear s_eqkt.
                  select single *
                  from eqkt
                  into s_eqkt
                    where equnr eq  wa_status_equnr-hequi.
                  wa_status_equnr-eqktx_ = s_eqkt-eqktx.
                  wa_status_equnr-eqp_inf = abap_true.

                  append wa_status_equnr to it_status_equnr.
                  clear wa_status_equnr.
                endloop.

                free it_status_hequi.
                move it_status_equnr to it_status_hequi.

                loop at it_status_hequi into wa_status_hequi where eqp_inf eq abap_true.

                  clear: gt_equz, lt_equz.
                  select *
                  from equz as a
                  inner join equi as b on b~equnr eq a~equnr
                  into corresponding fields of table gt_equz
                    where a~hequi eq wa_status_hequi-equnr
                      and a~datbi eq '99991231'.
*                  AND B~EQTYP EQ 'V'.

                  loop at gt_equz into lt_equz where hequi eq wa_status_hequi-equnr.
                    if sy-subrc = 0.
                      wa_status_equnr-equnr   = |{ lt_equz-equnr alpha = in }|. "Equipamento inferior
                      clear s_eqkt.
                      select single *
                      from eqkt
                      into s_eqkt
                        where equnr eq wa_status_equnr-equnr.
                      wa_status_equnr-eqktx = s_eqkt-eqktx.
                      wa_status_equnr-hequi   = lt_equz-hequi."Equipamento Superior

                      clear s_eqkt.
                      select single *
                      from eqkt
                      into s_eqkt
                        where equnr eq  wa_status_equnr-hequi.
                      wa_status_equnr-eqktx_ = s_eqkt-eqktx.
                      wa_status_equnr-eqp_inf = abap_true.

                      append wa_status_equnr to it_status_equnr.
                      clear: wa_status_equnr, lt_equz.
                    endif.
                  endloop.
                endloop.
              endif.
            endif.
*            ENDIF.
          endloop.


          sort it_saida_dev_equi ascending by equnr.
          sort it_status_equnr ascending by hequi.
          loop at it_saida_dev_equi into data(w_dev).
            loop at it_status_equnr into data(_status) where hequi = w_dev-equnr.
              if _status-eqp_inf is initial.
                delete it_status_equnr index sy-tabix.
                continue.
              endif.
            endloop.

            loop at it_status_equnr into data(status) where equnr = w_dev-equnr.
              if status-eqp_sup is initial.
                delete it_status_equnr index sy-tabix.
                continue.
              endif.
            endloop.
          endloop.

          clear p_devolucao.
          if it_status_equnr is initial.
            call screen 400 starting at 5 5 .
          else.
            p_devolucao = abap_true.
            call screen 300 starting at 5 5 .
          endif.
*
        endif.
      when 'BTN_SELECIONAR'.

        refresh: it_selected_rows.

        clear: c_destino, c_origem, wa_status_equnr, wa_saida_emprestimo_equi.

*** Inicio - Rubenilson - 23.12.24 - US138088
        wa_celltab-fieldname = 'CBX_ORD_ABAST'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        insert wa_celltab into lt_celltab index 1.

        wa_celltab-fieldname = 'CBX_ORD_REMON'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        insert wa_celltab into lt_celltab index 2.

        wa_celltab-fieldname = 'DEVOLUCAO_AUTOMATICA'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        insert wa_celltab into lt_celltab index 3.

        wa_celltab-fieldname = 'EQKTX'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        insert wa_celltab into lt_celltab index 4.

        wa_celltab-fieldname = 'EQUNR'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        insert wa_celltab into lt_celltab index 5.

        wa_celltab-fieldname = 'IWERK'.
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

        insert wa_celltab into lt_celltab index 6.
*** Fim - Rubenilson - 23.12.24 - US138088

        call method obj_alv_status->get_selected_rows
          importing
            et_index_rows = it_selected_rows.

        describe table it_selected_rows lines lines.

        if ( lines is initial ).
          message text-e01 type 'I' display like 'E'.
        else.

          sort it_selected_rows stable descending.

*-US 158036-26-11-2024-#158036-RJF-Início
          loop at it_selected_rows into wa_selected_rows.
            read table it_status_equnr into wa_status_equnr index wa_selected_rows-index.
            if wa_status_equnr-status eq icon_alert.
              data(lv_error) = abap_on.
              exit.
            endif.
          endloop.
          if lv_error eq abap_on.
            message wa_status_equnr-det type 'I' display like 'E'.
            exit.
          endif.
*-US 158036-26-11-2024-#158036-RJF-Fim

          loop at it_selected_rows into wa_selected_rows.
            read table it_status_equnr into wa_status_equnr index wa_selected_rows-index.

            "Verifica se o equipamento é superior ou inferior;
            if p_devolucao is initial.
              read table it_saida_emprestimo_equi into wa_saida_emprestimo_equi with key equnr = wa_status_equnr-equnr.

              "No caso de um eqpto inferior, iremos selecionar o superior, que
              "nesse caso é WA_STATUS_EQUNR-HEQUI;

              if ( sy-subrc is initial ).
                call function 'BAPI_EQUI_GETDETAIL'
                  exporting
                    equipment         = wa_status_equnr-hequi
                  importing
                    data_specific_exp = wa_data_specific_exp
                    data_general_exp  = wa_data_general.

                wa_saida_emprestimo_equi-equnr = wa_status_equnr-hequi.

                "No caso de um eqpto superior, iremos selecionar o inferior deste,
                "que neste caso é WA_STATUS_EQUNR-EQUNR;

              else.
                wa_status_equnr-equnr = |{ wa_status_equnr-equnr alpha = in }|.
                call function 'BAPI_EQUI_GETDETAIL'
                  exporting
                    equipment         = wa_status_equnr-equnr
                  importing
                    data_specific_exp = wa_data_specific_exp
                    data_general_exp  = wa_data_general.

                wa_saida_emprestimo_equi-equnr = wa_status_equnr-equnr.
              endif.

              wa_saida_emprestimo_equi-iwerk         = wa_data_general-maintplant.
              wa_saida_emprestimo_equi-eqktx         = wa_data_general-descript.
              wa_saida_emprestimo_equi-celltab       = lt_celltab. " Rubenilson - 23.12.24 - US138088
              select single *
              from equi
              into @data(ls_equi)
                where equnr eq @wa_saida_emprestimo_equi-equnr.

              if  ls_equi-eqtyp = 'V' or
                  ls_equi-eqtyp = 'A' or "FF - 05.04.24  - ins
                  ls_equi-eqtyp = '1' or "FF - 22.11.23  - ins
                  ls_equi-eqtyp = '2' or
                  ls_equi-eqtyp = '3' or
                  ls_equi-eqtyp = '4'.

                wa_saida_emprestimo_equi-cbx_ord_remon = 'X'.
              endif.

              if wa_status_equnr-eqp_sup is initial.
                wa_saida_emprestimo_equi-cbx_ord_abast = abap_false.
              else.
                wa_saida_emprestimo_equi-cbx_ord_abast = abap_true.
              endif.

              wa_status_equnr-equnr = |{ wa_status_equnr-equnr alpha = out }|.
              append wa_saida_emprestimo_equi to it_saida_emprestimo_equi.
              clear: ls_equi, wa_saida_emprestimo_equi.

              delete it_status_equnr where equnr = wa_status_equnr-equnr
                                       and hequi = wa_status_equnr-hequi.


              call method obj_alv_status->refresh_table_display
                exporting
                  is_stable = wa_stable.

              check ( it_status_equnr is initial ).
              sort it_saida_emprestimo_equi descending by equnr.
              delete adjacent duplicates from it_saida_emprestimo_equi comparing equnr.
              leave to screen 0200.


            else.

              if wa_status_equnr-eqp_inf is not initial.

                read table it_saida_equi_responsavel into wa_saida_equi_responsavel with key equnr = |{ wa_status_equnr-equnr alpha = out }|.
                if sy-subrc = 0.
                  move-corresponding wa_saida_equi_responsavel to wa_saida_dev_equi.
                  append wa_saida_dev_equi to it_saida_dev_equi.
                  delete it_status_equnr where equnr = |{ wa_saida_dev_equi-equnr alpha = in }|.
*                                       AND HEQUI = WA_STATUS_EQUNR-HEQUI.
                  clear: wa_saida_dev_equi, wa_saida_equi_responsavel.

                else.

                  move-corresponding wa_status_equnr  to wa_saida_dev_equi.
                  select single *
                  from zequi_emprestimo
                  into @data(_zequi_emprestimo)
                    where equnr eq @wa_status_equnr-hequi.

                  wa_saida_dev_equi-erdat       = sy-datum.
                  wa_saida_dev_equi-iwerk       = _zequi_emprestimo-iwerk.
                  wa_saida_dev_equi-cent_origem = _zequi_emprestimo-cent_origem.
                  append wa_saida_dev_equi to it_saida_dev_equi.
                  delete it_status_equnr where equnr = |{ wa_saida_dev_equi-equnr alpha = in }|.
*                                       AND HEQUI = WA_STATUS_EQUNR-HEQUI.
                  clear: wa_saida_dev_equi, wa_saida_equi_responsavel.
                endif.

              elseif wa_status_equnr-eqp_sup is not initial..
                read table it_saida_equi_responsavel into wa_saida_equi_responsavel with key equnr = |{ wa_status_equnr-hequi alpha = out }|.
                if sy-subrc = 0.
                  move-corresponding wa_saida_equi_responsavel to wa_saida_dev_equi.
                  append wa_saida_dev_equi to it_saida_dev_equi.
                  delete it_status_equnr where hequi = |{ wa_saida_dev_equi-equnr alpha = in }|.

*                                       AND HEQUI = WA_STATUS_EQUNR-HEQUI.
                  clear: wa_saida_dev_equi, wa_saida_equi_responsavel.
                endif.
              endif.
            endif.
          endloop.

          call method obj_alv_status->refresh_table_display
            exporting
              is_stable = wa_stable.

          check ( it_status_equnr is initial ).
          sort it_saida_dev_equi descending by equnr.


*         Adicionando por prioridade.
          data: cont type p decimals 1.
          clear cont.

          loop at it_saida_dev_equi assigning field-symbol(<dev_equi>).
            <dev_equi>-equnr = |{ <dev_equi>-equnr alpha = in }|.
            clear: lt_equz.
            select single *
            from equz as a
            inner join equi as b on b~equnr eq a~equnr
            into corresponding fields of lt_equz
              where a~equnr eq <dev_equi>-equnr
                and a~datbi eq '99991231'
                and b~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 e 05/04/2024 type A

            add 1 to cont.

            if lt_equz-equnr is not initial and lt_equz-hequi is initial .
              <dev_equi>-sequencia = 1.
              continue.
            endif.

            if lt_equz-equnr is not initial and lt_equz-hequi is not initial.
              <dev_equi>-sequencia = 2.
              continue.
            endif.

            if lt_equz is initial.
              <dev_equi>-sequencia = cont.
            endif.
          endloop.

          sort it_saida_dev_equi ascending by sequencia.
          delete adjacent duplicates from it_saida_dev_equi comparing equnr.
          leave to screen 0400.
        endif.


      when 'BTN_DESMONTAR'.
        refresh: it_selected_rows.

        call method obj_alv_status->get_selected_rows
          importing
            et_index_rows = it_selected_rows.

        describe table it_selected_rows lines lines.

        if ( lines is initial ).
          message text-e01 type 'I' display like 'E'.
        else.
          sort it_selected_rows stable descending.
          if p_devolucao is initial.
            loop at it_selected_rows into wa_selected_rows.
              read table it_status_equnr into wa_status_equnr index wa_selected_rows-index.

              "Verifica se o equipamento é superior ou inferior;

              read table it_saida_emprestimo_equi into wa_saida_emprestimo_equi
                with key equnr = wa_status_equnr-hequi.

              "No caso de um eqpto inferior, iremos selecionar o superior, que
              "nesse caso é WA_STATUS_EQUNR-HEQUI;

              if ( sy-subrc is initial ).
                call function 'BAPI_EQMT_DISMANTLEHR'
                  exporting
                    equipment = wa_status_equnr-equnr
                    superequi = wa_status_equnr-hequi
                    date      = sy-datlo
                    time      = sy-timlo
                  importing
                    return    = wa_return.

                "No caso de um eqpto superior, iremos selecionar o inferior deste,
                "que neste caso é WA_STATUS_EQUNR-EQUNR;

              else.
                call function 'BAPI_EQMT_DISMANTLEHR'
                  exporting
                    equipment = wa_status_equnr-hequi
                    superequi = wa_status_equnr-equnr
                    date      = sy-datlo
                    time      = sy-timlo
                  importing
                    return    = wa_return.
              endif.

              delete it_status_equnr where equnr = wa_status_equnr-equnr
                                       and hequi = wa_status_equnr-hequi.
            endloop.

            call method obj_alv_status->refresh_table_display
              exporting
                is_stable = wa_stable.

            check ( it_status_equnr is initial ).
            leave to screen 0200.
          else.

            loop at it_selected_rows into wa_selected_rows.
              read table it_status_equnr into wa_status_equnr index wa_selected_rows-index.

              "Verifica se o equipamento é superior ou inferior;
              read table it_saida_dev_equi into wa_saida_dev_equi with key equnr = wa_status_equnr-hequi.

              "No caso de um eqpto inferior, iremos selecionar o superior, que
              "nesse caso é WA_STATUS_EQUNR-HEQUI;

              if ( sy-subrc is initial ).
                call function 'BAPI_EQMT_DISMANTLEHR'
                  exporting
                    equipment = wa_status_equnr-equnr
                    superequi = wa_status_equnr-hequi
                    date      = sy-datlo
                    time      = sy-timlo
                  importing
                    return    = wa_return.

                "No caso de um eqpto superior, iremos selecionar o inferior deste,
                "que neste caso é WA_STATUS_EQUNR-EQUNR;

              else.
                call function 'BAPI_EQMT_DISMANTLEHR'
                  exporting
                    equipment = wa_status_equnr-hequi
                    superequi = wa_status_equnr-equnr
                    date      = sy-datlo
                    time      = sy-timlo
                  importing
                    return    = wa_return.
              endif.

              delete it_status_equnr where equnr = wa_status_equnr-equnr
                                       and hequi = wa_status_equnr-hequi.
            endloop.

            call method obj_alv_status->refresh_table_display
              exporting
                is_stable = wa_stable.

            check ( it_status_equnr is initial ).
            leave to screen 0400.
          endif.
        endif.

      when 'BTN_ABOUT'.
        message i836(sd) with 'É necessário selecionar ou desmontar o equipamento'
                              'inferior/superior para continuar com o empréstimo/devolução.'.

      when 'BTN_CHECK_EQ_INF'.
        refresh: it_selected_rows.
        data: zt_equz type equz.

        call method obj_alv_0130->get_selected_rows
          importing
            et_index_rows = it_selected_rows.

        describe table it_selected_rows lines lines.

        if ( lines is initial ).
          message text-e01 type 'I' display like 'E'.
        else.
          loop at it_selected_rows into wa_selected_rows.
            read table it_saida_equi_responsavel into wa_saida_equi_responsavel index wa_selected_rows-index.
            clear wa_zequi_emprestimo.
            select single *
              from zequi_emprestimo
              into wa_zequi_emprestimo
             where equnr = wa_saida_equi_responsavel-equnr.

            if wa_zequi_emprestimo is not initial.
              select *
              from equz as a
              inner join equi as b on b~equnr eq a~equnr
              into corresponding fields of table gt_equz
                where a~hequi eq wa_saida_equi_responsavel-equnr
                  and a~datbi eq '99991231'
                  and b~eqtyp in ( 'A', 'V', '1', '2', '3', '4' ). "FF - 22.11.2023 - ins e "FF - 05.04.2023 - ins
            endif.
          endloop.
          data(sy_ucomm) = 'BTN_CHECK_EQ_INF'.
        endif.


        call screen 0100.
*        CALL METHOD OBJ_ALV_0130->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = WA_STABLE.



    endcase.
  endmethod.                    "GET_UCOMM

endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION
"LCL_EVENT_HANDLER IMPLEMENTATION

module input_001 input.
*  CALL METHOD MAIN_INSTANCE->USER_COMMAND
*    EXPORTING
*      UCOMM          = SY-UCOMM
*    EXCEPTIONS
*      INCORRECT_DATA = 4.
endmodule.
