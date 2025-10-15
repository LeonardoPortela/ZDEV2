function zpm_dispara_api_ordem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(HEADER) TYPE  ZCAUFVDB OPTIONAL
*"     VALUE(OPERATION) TYPE  ZAFVGB OPTIONAL
*"     VALUE(CONFIRMATION) TYPE  ZAFRUB OPTIONAL
*"     VALUE(ID_EQUIPE) TYPE  ZIDEQP OPTIONAL
*"     VALUE(ORDEM_ORC) TYPE  ZTPM_D_M_ORDEM_T OPTIONAL
*"  TABLES
*"      T_LIST STRUCTURE  BAPI_ALM_ORDER_OBJECTLIST OPTIONAL
*"      T_RIWOL STRUCTURE  RIWOL OPTIONAL
*"----------------------------------------------------------------------


  data: lt_ordem        type table of ztpm_d_m_ordem,
        lv_created      type timestampl,
        lv_updated      type timestampl,
        lv_sobra        type string,
        lv_string       type string,
        lt_operacao     type ztpm_d_m_operacao_t,
        lw_ordem        type ztpm_d_m_ordem,
        lv_line         type bsvx-sttxt,
        lw_header       type bapi_alm_order_header_e,
        lt_operations   type table of bapi_alm_order_operation_e,
        lt_return       type table of bapiret2,
        lt_apontamentos type table of ztpm_d_m_apont,
        lt_riwol        type table of riwol,
        lt_riwol_upd    type table of riwol,
        lt_ripwo_ex     type table of ripw0,
        lt_briwol_mem   type table of riwol,
        lt_iser02       type table of rserxx,
        lt_objk         type table of objk,
        lv_objnr        type ihsg-objnr,
        lv_prox_aprov   type string,
        ls_ordem        type zpm_ordem_orc,
        lv_nota_ordem   type qmel-qmnum.

  data: lt_excluir_oper  type table of ztpm_d_m_ordem. " #106405  FF  14.03.2023

  "Desativar api mobman/ BUG SOLTO 187013 / AOENNING / 31/07/2025.
  exit.
  "Desativar api mobman/ BUG SOLTO 187013 / AOENNING / 31/07/2025.




  if ordem_orc is not initial.
    read table ordem_orc assigning field-symbol(<f_ordem>) index 1.
    if sy-subrc is initial.

      select single *
      from zpmt0064
      into @data(ls_0064)
      where objeto = 'ORDEM'
        and tipo   = @<f_ordem>-auart
        and centro = @<f_ordem>-iwerk
        and campo  = 'USER4'.
      if sy-subrc is initial.
        if ls_0064-obrig is not initial.
          lv_objnr = 'OR' && <f_ordem>-aufnr.

          select *
            from ihsg
            into table @data(lt_ihsg)
            where objnr = @lv_objnr.
          if sy-subrc is initial.
            sort lt_ihsg by pmsog.
            delete adjacent duplicates from lt_ihsg comparing pmsog.

            select *
              from zpmr0002
              into table @data(lt_zpmt0002)
              for all entries in @lt_ihsg
              where centro_desp = @<f_ordem>-iwerk
                and permit      = @lt_ihsg-pmsog
                and nivel       = '0000000001'.
            if sy-subrc is initial.
              sort lt_zpmt0002 by nivel.

              loop at lt_zpmt0002 assigning field-symbol(<fs_aprov>).

                if sy-tabix = 1.

                  if <fs_aprov>-usua_subst is not initial and <fs_aprov>-data_lim >= sy-datum.
                    concatenate <fs_aprov>-usua_subst ',' into lv_prox_aprov.
                  else.
                    concatenate <fs_aprov>-aprovador ',' into lv_prox_aprov.
                  endif.

                else.

                  if <fs_aprov>-usua_subst is not initial and <fs_aprov>-data_lim >= sy-datum.
                    concatenate <fs_aprov>-usua_subst ',' lv_prox_aprov into lv_prox_aprov.
                  else.
                    concatenate <fs_aprov>-aprovador ',' lv_prox_aprov into lv_prox_aprov.
                  endif.

                endif.

              endloop.

              ls_ordem-aufnr        = <f_ordem>-aufnr.
              ls_ordem-ktext        = <f_ordem>-ktext.
              ls_ordem-equnr        = <f_ordem>-equnr.
              ls_ordem-erdat        = <f_ordem>-erdat.
              ls_ordem-user4        = <f_ordem>-user4.
              ls_ordem-prox_aprov   = lv_prox_aprov.

              try .
                  zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem ).
                catch zcx_integracao into data(zcx_integracao).
                  message id zcx_integracao->zif_error~msgid type 'I'
                   number zcx_integracao->zif_error~msgno
                     with zcx_integracao->zif_error~msgv1
                          zcx_integracao->zif_error~msgv2
                          zcx_integracao->zif_error~msgv3
                          zcx_integracao->zif_error~msgv4.
                catch zcx_error into data(zcx_error).
                  message id zcx_error->zif_error~msgid type 'I'
                   number zcx_error->zif_error~msgno
                     with zcx_error->zif_error~msgv1
                          zcx_error->zif_error~msgv2
                          zcx_error->zif_error~msgv3
                          zcx_error->zif_error~msgv4.

              endtry.

            endif.

          endif.

        endif.

      endif.

    endif.

    exit.

  endif.

  if confirmation is initial.

    data(lt_operacao_aux) = operation.
    sort lt_operacao_aux by arbid.
    delete adjacent duplicates from lt_operacao_aux comparing arbid.

    select objid,arbpl
      from crhd
      into table @data(lt_crhd)
      for all entries in @lt_operacao_aux
      where objty = 'A'
        and objid = @lt_operacao_aux-arbid.
    if sy-subrc is initial.
      sort lt_crhd by objid.
    endif.

    read table header assigning field-symbol(<fs_header>) index 1.
    if sy-subrc is initial.
      move-corresponding <fs_header> to lw_ordem.
      if id_equipe is not initial and id_equipe <> '00000'.
        lw_ordem-idequipe = id_equipe.
      endif.

      if t_list[] is initial.

        loop at t_riwol assigning field-symbol(<fs_riwol>).
          append initial line to lw_ordem-notas assigning field-symbol(<fs_notas>).
          if <fs_riwol>-ihnum(1) eq '%'.

            lv_nota_ordem = <fs_header>-qmnum.
            <fs_notas>-qmnum = <fs_header>-qmnum.
          else.
            <fs_notas>-qmnum =  <fs_riwol>-ihnum.
          endif.

        endloop.

      else.

        sort t_riwol by ihnum.

        loop at t_list assigning field-symbol(<fs_list>).

          read table t_riwol transporting no fields
          with key ihnum = <fs_list>-notif_no
          binary search.
          if sy-subrc is not initial.

            append initial line to lw_ordem-notas assigning <fs_notas>.
            <fs_notas>-qmnum =  <fs_list>-notif_no.
            <fs_notas>-desvincular = abap_true.

          endif.

        endloop.

        sort t_list by notif_no.

        loop at t_riwol assigning <fs_riwol>.

          read table t_list transporting no fields
          with key notif_no = <fs_riwol>-ihnum
          binary search.
          if sy-subrc is not initial.

            append initial line to lw_ordem-notas assigning <fs_notas>.
            <fs_notas>-qmnum =  <fs_riwol>-ihnum.

          endif.

        endloop.

      endif.

      select single arbpl
        from crhd
        into lw_ordem-arbpl
        where objty = 'A'
          and objid = <fs_header>-gewrk.

      call function 'STATUS_TEXT_EDIT'
        exporting
          objnr            = lw_ordem-objnr
          spras            = 'P'
        importing
          line             = lv_line
        exceptions
          object_not_found = 1
          others           = 2.
      if sy-subrc = 0.

        if lv_line cs 'ABER'.
          lw_ordem-istat = '0'.
        elseif lv_line cs 'LIB'.
          lw_ordem-istat = '1'.
        elseif lv_line cs 'ENTE' or
               lv_line cs 'ENCE' .
          lw_ordem-istat = '2'.
        endif.
      endif.

      if lw_ordem-pernr eq '00000000'.
        clear lw_ordem-pernr.
      endif.

      loop at operation assigning field-symbol(<fs_operation1>).

**  Begin of    #106405  FF  14.03.2023
        "<fs_operation1>-phflg = 'X' --> Ordens liberadas
        "<fs_operation1>-vbkz = 'D'--> Ordem não liberadas
        if <fs_operation1>-phflg = 'X' or <fs_operation1>-vbkz = 'D'. "Operação deletada

          append initial line to lt_excluir_oper assigning field-symbol(<fs_del>).
          move-corresponding <fs_operation1> to <fs_del>.
          <fs_del>-phflg = 'X'.
          <fs_del>-aufnr = lw_ordem-aufnr.
        endif.
** End of FF  14.03.2023

        append initial line to lt_operacao assigning field-symbol(<fs_operacao1>).

        move-corresponding <fs_operation1> to <fs_operacao1>.

        if <fs_operacao1>-pernr eq '00000000'.
          clear <fs_operacao1>-pernr .
        endif.

        <fs_operacao1>-aufnr = lw_ordem-aufnr.

        read table lt_crhd assigning field-symbol(<fs_crhd>)
        with key objid = <fs_operation1>-arbid
        binary search.
        if sy-subrc is initial.
          <fs_operacao1>-arbpl = <fs_crhd>-arbpl.
        endif.
      endloop.

      if lt_operacao is not initial.
        lw_ordem-operacao = lt_operacao.
      endif.

      if lw_ordem-erdat is not initial and lw_ordem-erfzeit is not initial.
        call function 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          exporting
            i_date = lw_ordem-erdat    " Data
            i_time = lw_ordem-erfzeit  " Hora
          importing
            e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_created.
        replace all occurrences of '.' in lv_string with space.
        condense lv_string no-gaps.

        lw_ordem-created_at = lv_string(13).

      endif.

      if lw_ordem-aedat is not initial and lw_ordem-aezeit is not initial.
        call function 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          exporting
            i_date = lw_ordem-aedat    " Data
            i_time = lw_ordem-aezeit  " Hora
          importing
            e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string  = lv_updated.
        replace all occurrences of '.' in lv_string with space.
        condense lv_string no-gaps.

        lw_ordem-updated_at = lv_string(13).

      endif.

    endif.

    if lv_nota_ordem is not initial.

      data: lw_nota        type zepm_d_nota,
            lt_objnr       type ztpm_d_n_catalag_t,
            t_texto        type table of tline,
            w_texto        type tline,
            l_name         type thead-tdname,
*          lv_created TYPE timestampl,
*          lv_updated TYPE timestampl,
*          lv_string  TYPE string,
            lv_ausvn       type viqmel-ausvn,
            lv_auztv       type viqmel-auztv,
            lv_msg         type string,
            lv_line_status type bsvx-sttxt,
            lt_qmfe        type table of wqmfe,
            lt_qmma        type table of wqmma,
            lt_qmsm        type table of wqmsm,
            lt_qmur        type table of wqmur,
            ls_qmel        type viqmel,
            lv_id_integra  type zde_id_integracao.

      call function 'IQS4_GET_NOTIFICATION'
        exporting
          i_qmnum     = lv_nota_ordem
        importing
          e_viqmel    = ls_qmel
        tables
          e_iviqmfe_t = lt_qmfe
          e_iviqmma_t = lt_qmma
          e_iviqmsm_t = lt_qmsm
          e_iviqmur_t = lt_qmur.

      if ls_qmel-qmnum is not initial.

        if sy-subrc is initial.
          move-corresponding ls_qmel to lw_nota.

          if lw_nota-ausvn eq lv_ausvn.
            lw_nota-ausvn = sy-datum.
          endif.

          if lw_nota-auztv eq lv_auztv.
            lw_nota-auztv = sy-uzeit.
          endif.

          if ls_qmel-arbpl is not initial.

            select single arbpl
              from crhd
              into lw_nota-arbpl
              where objid = ls_qmel-arbpl.

          endif.

          call function 'STATUS_TEXT_EDIT'
            exporting
              objnr            = <fs_header>-objnr
              only_active      = ' '
              spras            = sy-langu
            importing
              line             = lv_line
            exceptions
              object_not_found = 1
              others           = 2.

          lw_nota-istat = '1'.

          l_name = ls_qmel-qmnum.

          call function 'READ_TEXT'
            exporting
              id                      = 'LTXT'
              language                = sy-langu
              name                    = l_name
              object                  = 'QMEL'
            tables
              lines                   = t_texto
            exceptions
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              others                  = 8.

          loop at t_texto assigning field-symbol(<_text_l>).
            if lw_nota-txtnt is initial.
              lw_nota-txtnt =  | ->{ <_text_l>-tdline }| .
            else.
              lw_nota-txtnt = |{ lw_nota-txtnt } ->{ <_text_l>-tdline }|.
            endif.
          endloop.


          if ls_qmel-erdat is not initial.
            call function 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
              exporting
                i_date = ls_qmel-erdat   " Data
                i_time = ls_qmel-erzeit
              importing
                e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

            lv_string = lv_created.
            replace all occurrences of '.' in lv_string with space.
            condense lv_string no-gaps.

            lw_nota-created_at = lv_string(13).

          endif.

          if ls_qmel-aedat is not initial.
            call function 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
              exporting
                i_date = ls_qmel-aedat   " Data
                i_time = ls_qmel-aezeit
              importing
                e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

            lv_string = lv_updated.
            replace all occurrences of '.' in lv_string with space.
            condense lv_string no-gaps.

            lw_nota-updated_at = lv_string(13).

          endif.

        endif.


        select *
          from tqscr
          into table @data(t_tqscr)
          where qmart eq @ls_qmel-qmart
            and qmtyp eq '01'
            and tabcd eq '10\TAB00'.
        sort t_tqscr[] by sub03 ascending.
        delete t_tqscr[] where sub03 ne '035'.
        sort t_tqscr[] by qmart qmtyp tabcd ascending.

        loop at lt_qmfe assigning field-symbol(<fs_qmfe_line>).
          append initial line to lt_objnr assigning field-symbol(<fs_objnr>).

          read table t_tqscr assigning field-symbol(<_tqscr>)
          with key qmart = lw_nota-qmart
                   qmtyp = '01'
                   tabcd = '10\TAB00'
          binary search.
          if sy-subrc is initial.

            <fs_objnr>-numero_item = <fs_qmfe_line>-fenum.
            <fs_objnr>-otkat = <fs_qmfe_line>-otkat.
            <fs_objnr>-otgrp = <fs_qmfe_line>-otgrp.
            <fs_objnr>-oteil = <fs_qmfe_line>-oteil.
            <fs_objnr>-fekat = <fs_qmfe_line>-fekat.
            <fs_objnr>-fegrp = <fs_qmfe_line>-fegrp.
            <fs_objnr>-fecod = <fs_qmfe_line>-fecod.
            <fs_objnr>-fetxt = <fs_qmfe_line>-fetxt.

            if <fs_qmfe_line>-aeknz eq 'D' .

              <fs_objnr>-eliminado = '1'.

            else.

              <fs_objnr>-eliminado = '0'.

            endif.

            if lt_qmur is not initial.
              read table lt_qmur assigning field-symbol(<fs_qmur_line>)
              with key qmnum = <fs_qmfe_line>-qmnum
                       fenum = <fs_qmfe_line>-fenum
              binary search.
              if sy-subrc is initial.
                <fs_objnr>-urkat = <fs_qmur_line>-urkat.
                <fs_objnr>-urgrp = <fs_qmur_line>-urgrp.
                <fs_objnr>-urcod = <fs_qmur_line>-urcod.
                <fs_objnr>-urstx = <fs_qmur_line>-urtxt.
              endif.
            endif.
          else.

            loop at lt_qmma assigning field-symbol(<fs_qmma_line>).

              <fs_objnr>-numero_item = <fs_qmma_line>-manum.
              <fs_objnr>-mnkat =  <fs_qmma_line>-mnkat.
              <fs_objnr>-mngrp =  <fs_qmma_line>-mngrp.
              <fs_objnr>-mncod =  <fs_qmma_line>-mncod.

              if <fs_qmma_line>-aeknz eq 'D'.
                <fs_objnr>-eliminado = '1'.
              else.
                <fs_objnr>-eliminado = '0'.
              endif.

            endloop.

          endif.

        endloop.

        lw_nota-part_objnr = lt_objnr.

        try.

            zcl_cria_modifica_nota_mobman=>zif_cria_modifica_nota_mobman~get_instance(
                  )->set_dados_nota( i_data = lw_nota
                  )->post_cria_modifica_nota_mobman( exporting i_nota = lw_nota ).

          catch zcx_integracao into data(ex_integra).    "
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

          catch zcx_error into data(ex_error).    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        endtry.
*
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 INTO lv_msg.
*      CONCATENATE lv_msg 'Nota' lw_nota-qmnum INTO lv_msg SEPARATED BY space.
*
*      MESSAGE lv_msg TYPE 'S'.
      endif.

    endif.

    wait up to 3 seconds.


    """""""""""EXCLUIR OPERAÇÃO"""""""""""""""""""""""""""""""""""""
**  Begin of    #106405  FF  14.03.2023

    loop at lt_excluir_oper assigning <fs_del>.

      <fs_del>-aufnr = lw_ordem-aufnr.

      try.
          zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
                )->set_dados_ordem( i_data = <fs_del>
                )->post_excluir_oper( exporting i_ordem = <fs_del> ).

        catch zcx_integracao into ex_integra.    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        catch zcx_error into ex_error.    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      endtry.

    endloop.


    try.

        zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
              )->set_dados_ordem( i_data = lw_ordem
              )->post_cria_modifica_ordem_app( exporting i_ordem = lw_ordem ).

      catch zcx_integracao into ex_integra.    "
        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      catch zcx_error into ex_error.    "  "
        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

    endtry.


    if id_equipe is initial.


**  Begin of  #105926  FF   06.03.2023
      try.
          zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
                )->set_dados_ordem( i_data = lw_ordem
                )->post_atualiza_status_ordem( exporting i_ordem = lw_ordem ).

        catch zcx_integracao into ex_integra.    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        catch zcx_error into ex_error.    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      endtry.
**  End of  FF   06.03.2023

      select single *
        from zpmt0064
        into ls_0064
        where objeto = 'ORDEM'
          and tipo   = lw_ordem-auart
          and centro = lw_ordem-iwerk
          and campo  = 'USER4'.
      if sy-subrc is initial.
        if ls_0064-obrig is not initial.
          lv_objnr = 'OR' && lw_ordem-aufnr.

          select *
            from ihsg
            into table lt_ihsg
            where objnr = lv_objnr.
          if sy-subrc is initial.
            sort lt_ihsg by pmsog.
            delete adjacent duplicates from lt_ihsg comparing pmsog.

            select *
              from zpmr0002
              into table lt_zpmt0002
              for all entries in lt_ihsg
              where centro_desp = lw_ordem-iwerk
                and permit      = lt_ihsg-pmsog
                and nivel       = '0000000001'.
            if sy-subrc is initial.
              sort lt_zpmt0002 by nivel.

              loop at lt_zpmt0002 assigning <fs_aprov>.

                if sy-tabix = 1.

                  if <fs_aprov>-usua_subst is not initial and <fs_aprov>-data_lim >= sy-datum.
                    concatenate <fs_aprov>-usua_subst ',' into lv_prox_aprov.
                  else.
                    concatenate <fs_aprov>-aprovador ',' into lv_prox_aprov.
                  endif.

                else.

                  if <fs_aprov>-usua_subst is not initial and <fs_aprov>-data_lim >= sy-datum.
                    concatenate <fs_aprov>-usua_subst ',' lv_prox_aprov into lv_prox_aprov.
                  else.
                    concatenate <fs_aprov>-aprovador ',' lv_prox_aprov into lv_prox_aprov.
                  endif.

                endif.

              endloop.

              ls_ordem-aufnr        = lw_ordem-aufnr.
              ls_ordem-ktext        = lw_ordem-ktext.
              ls_ordem-equnr        = lw_ordem-equnr.
              ls_ordem-erdat        = lw_ordem-erdat.
              ls_ordem-user4        = lw_ordem-user4.
              ls_ordem-prox_aprov   = lv_prox_aprov.

              try .
                  zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem ).
                catch zcx_integracao into zcx_integracao.
                  message id zcx_integracao->zif_error~msgid type 'I'
                   number zcx_integracao->zif_error~msgno
                     with zcx_integracao->zif_error~msgv1
                          zcx_integracao->zif_error~msgv2
                          zcx_integracao->zif_error~msgv3
                          zcx_integracao->zif_error~msgv4.
                catch zcx_error into zcx_error.
                  message id zcx_error->zif_error~msgid type 'I'
                   number zcx_error->zif_error~msgno
                     with zcx_error->zif_error~msgv1
                          zcx_error->zif_error~msgv2
                          zcx_error->zif_error~msgv3
                          zcx_error->zif_error~msgv4.

              endtry.

            endif.

          endif.

        endif.

      endif.

    endif.

  else.

    read table confirmation assigning field-symbol(<fs_confirmation>) index 1.
    if sy-subrc is initial.

      call function 'BAPI_ALM_ORDER_GET_DETAIL'
        exporting
          number        = <fs_confirmation>-aufnr
        importing
          es_header     = lw_header
        tables
          et_operations = lt_operations
          return        = lt_return.

      if not line_exists( lt_return[ type = 'E' ] ).

        call function 'STATUS_TEXT_EDIT'
          exporting
            objnr            = lw_header-object_no
            spras            = 'P'
          importing
            line             = lv_line
          exceptions
            object_not_found = 1
            others           = 2.
        if sy-subrc = 0.

          if lv_line cs 'ABER'.
            lw_ordem-istat = '0'.
          elseif lv_line cs 'LIB'.
            lw_ordem-istat = '1'.
          elseif lv_line cs 'ENTE' or
                 lv_line cs 'ENCE' .
            lw_ordem-istat = '2'.
          endif.
        endif.

        lw_ordem-aufnr      = lw_header-orderid.
        lw_ordem-auart      = lw_header-order_type.
        lw_ordem-qmnum      = lw_header-notif_no.
        lw_ordem-ktext      = lw_header-short_text.
        lw_ordem-iwerk      = lw_header-planplant.
        lw_ordem-ingpr      = lw_header-plangroup.
        lw_ordem-arbpl      = lw_header-mn_wk_ctr.
        lw_ordem-user4      = lw_header-estimated_costs.
        lw_ordem-ilart      = lw_header-pmacttype.
*lw_ordem-idequipe   =
        lw_ordem-anlzu      = lw_header-systcond.
        lw_ordem-priok      = lw_header-priority.
        lw_ordem-tplnr      = lw_header-funct_loc.
        lw_ordem-equnr      = lw_header-equipment.
        lw_ordem-gstrp      = lw_header-start_date.
        lw_ordem-gltrp      = lw_header-finish_date.
        lw_ordem-erdat      = lw_header-enter_date.


        loop at lt_operations assigning field-symbol(<fs_operation>).
          append initial line to lt_operacao assigning field-symbol(<fs_operacao>).

          <fs_operacao>-vornr = <fs_operation>-activity.
          <fs_operacao>-qmnum = <fs_operation>-notif_no. "FF - 16/04/24 #131818
          <fs_operacao>-ltxa1 = <fs_operation>-description.
          <fs_operacao>-aufnr = lw_header-orderid.
          <fs_operacao>-werks = <fs_operation>-plant.
          <fs_operacao>-arbpl = <fs_operation>-work_cntr.
          <fs_operacao>-steus = <fs_operation>-control_key.
          <fs_operacao>-arbei = <fs_operation>-work_activity.
          <fs_operacao>-arbeh = <fs_operation>-un_work.
          <fs_operacao>-anzzl = <fs_operation>-number_of_capacities.
          <fs_operacao>-pernr = <fs_operation>-pers_no.
          <fs_operacao>-einsa = <fs_operation>-constraint_type_start.
          <fs_operacao>-einse = <fs_operation>-constraint_type_finish.
          <fs_operacao>-ntanf = <fs_operation>-start_cons.
          <fs_operacao>-ntend = <fs_operation>-fin_constr.

          read table confirmation assigning field-symbol(<fs_time>)
          with key vornr = <fs_operacao>-vornr
          binary search.
          if sy-subrc is initial.
            loop at confirmation assigning <fs_time> from sy-tabix.
              if <fs_operacao>-vornr <> <fs_time>-vornr.
                exit.
              endif.

              append initial line to lt_apontamentos assigning field-symbol(<fs_apontamentos>).

              move-corresponding <fs_time> to <fs_apontamentos>.

              call function 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
                exporting
                  i_date = sy-datum    " Data
                  i_time = sy-uzeit  " Hora
                importing
                  e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

              lv_string = lv_created.
              replace all occurrences of '.' in lv_string with space.
              condense lv_string no-gaps.

              <fs_apontamentos>-created_at = lv_string(13).


            endloop.

            <fs_operacao>-apontamentos = lt_apontamentos.

          endif.

        endloop.

        if lt_operacao is not initial.
          lw_ordem-operacao = lt_operacao.
        endif.

        if lw_ordem-erdat is not initial and lw_ordem-erfzeit is not initial.
          call function 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            exporting
              i_date = lw_ordem-erdat    " Data
              i_time = lw_ordem-erfzeit  " Hora
            importing
              e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_created.
          replace all occurrences of '.' in lv_string with space.
          condense lv_string no-gaps.

          lw_ordem-created_at = lv_string(13).

        endif.

        if lw_ordem-aedat is not initial and lw_ordem-aezeit is not initial.
          call function 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            exporting
              i_date = lw_ordem-aedat    " Data
              i_time = lw_ordem-aezeit  " Hora
            importing
              e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string  = lv_updated.
          replace all occurrences of '.' in lv_string with space.
          condense lv_string no-gaps.

          lw_ordem-updated_at = lv_string(13).

        endif.


        try.

            zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
                  )->set_dados_ordem( i_data = lw_ordem
                  )->post_cria_modifica_ordem_app( exporting i_ordem = lw_ordem ).

          catch zcx_integracao into ex_integra.    "
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

          catch zcx_error into ex_error.    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        endtry.


**  Begin of  #105926  FF   06.03.2023
        try.

            zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
                  )->set_dados_ordem( i_data = lw_ordem
                  )->post_atualiza_status_ordem( exporting i_ordem = lw_ordem ).

          catch zcx_integracao into ex_integra.    "
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

          catch zcx_error into ex_error.    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        endtry.
** End of FF  06.03.2023

      endif.

    endif.

  endif.
*
*  """""""""""EXCLUIR OPERAÇÃO"""""""""""""""""""""""""""""""""""""
***  Begin of    #106405  FF  14.03.2023
*  LOOP AT lt_excluir_oper ASSIGNING <fs_del>.
*
*    <fs_del>-aufnr = lw_ordem-aufnr.
*
*    TRY.
*        zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
*              )->set_dados_ordem( i_data = <fs_del>
*              )->post_excluir_oper( EXPORTING i_ordem = <fs_del> ).
*
*      CATCH zcx_integracao INTO ex_integra.    "
*        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*
*      CATCH zcx_error INTO ex_error.    "  "
*        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*
*    ENDTRY.
*
*  ENDLOOP.

  """""""""""ESTORNO APONTAMENTO"""""""""""""""""""""""""""""""""""""
  read table confirmation assigning field-symbol(<fs>) index 1.
  if sy-subrc is initial.
    select * from afru
      into table @data(lt_afru)
      where rueck = @<fs>-rueck
        and rmzhl = @<fs>-rmzhl
        and stokz = 'X'.  "Apontamento estornado

    if sy-subrc <> 0.
      clear lt_afru.
    endif.
  endif.

  loop at lt_afru assigning field-symbol(<fs_afru>).

    try.
        zcl_cria_modifica_ordem_mobman=>zif_cria_modifica_ordem_mobman~get_instance(
              )->set_dados_confirmation( i_data = <fs_afru>
              )->post_estornar_apont( exporting i_afru = <fs_afru> ). "AFRU --> apontamento

      catch zcx_integracao into ex_integra.    "
        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      catch zcx_error into ex_error.    "  "
        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

    endtry.

  endloop.
** End of FF  14.03.2023


endfunction.
