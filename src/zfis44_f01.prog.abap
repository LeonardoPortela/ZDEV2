*&---------------------------------------------------------------------*
*& Include          ZFIS44_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_filial_conf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_filial_conf .

  types: begin of ty_proc,
           chave      type zib_nfe_forn-nu_chave,
           dt_emissao type zib_nfe_dist_ter-dt_emissao,
           dest_cnpj  type stcd1,
           dest_ie    type stcd3,
           bukrs      type zib_nfe_forn-bukrs,
           branch     type zib_nfe_forn-branch,
         end of ty_proc.


  data: lr_destino_cnpj   type range of j_1bbranch-stcd1,
        lr_notas_d        type range of j_1bnfdoc-nftype,
        lr_notas_s        type range of j_1bnfdoc-nftype,
        lv_data_par       type sy-datum,
        lv_hora_lim       type sy-uzeit,
        lt_proc           type table of ty_proc,
        lo_manifesto_dest type ref to zcl_manifesto_dest,
        lv_dt_emissao     type sy-datum.


  lv_dt_emissao = sy-datum - 180.

  select *
    from tvarvc
    into table @data(lt_tvarvc)
    where ( name = 'Z_FIS_NOTAS_DIVERSAS' or
            name = 'Z_FIS_NOTAS_SIGAM' ).
  if sy-subrc is initial.
    sort lt_tvarvc by name.

    loop at lt_tvarvc assigning field-symbol(<fs_tvarvc>).
      if <fs_tvarvc>-name = 'Z_FIS_NOTAS_DIVERSAS'.
        append initial line to lr_notas_d assigning field-symbol(<fs_notas_d>).
        <fs_notas_d>-sign   = 'I'.
        <fs_notas_d>-option = 'EQ'.
        <fs_notas_d>-low    = <fs_tvarvc>-low.
      endif.

      if <fs_tvarvc>-name = 'Z_FIS_NOTAS_SIGAM'.
        append initial line to lr_notas_s assigning field-symbol(<fs_notas_s>).
        <fs_notas_s>-sign   = 'I'.
        <fs_notas_s>-option = 'EQ'.
        <fs_notas_s>-low    = <fs_tvarvc>-low.
      endif.

    endloop.
  endif.

  select a~model,a~dt_emissao,a~belnr,a~docnum_nfe,a~destino_cnpj,a~chave_nfe,a~destino_ie, b~docnum
    from zib_nfe_dist_ter as a inner join zib_nfe_forn as b
      on b~nu_chave = a~chave_nfe
     and b~docnum <> '' inner join j_1bnfdoc as c
      on c~docnum = b~docnum
     and c~pstdat > @lv_dt_emissao
      into table @data(lt_nfe_dist)
      where a~model         = '55'
        and  not exists ( select *  from
                            zsdt0127 as d
                            where a~chave_nfe = d~chave
                              and d~autorizado = @abap_true ).

*  SELECT a~model,a~dt_emissao,a~belnr,a~docnum_nfe,a~destino_cnpj,a~chave_nfe,a~destino_ie
*    FROM zib_nfe_dist_ter AS a
*    INTO TABLE @DATA(lt_nfe_dist)
*    WHERE a~model         = '55'
*      AND a~dt_emissao    > @lv_dt_emissao
*      AND ( a~belnr      <> @abap_false
*       OR   a~docnum_nfe <> @abap_false )
*      AND  NOT EXISTS ( SELECT *  FROM
*                          zsdt0127 AS b
*                          WHERE a~chave_nfe = b~chave
*                          AND b~autorizado = @abap_true ).
  if sy-subrc is initial.

    delete adjacent duplicates from lt_nfe_dist comparing all fields.

    data(lt_nfe_dist_aux) = lt_nfe_dist.
    sort lt_nfe_dist_aux by destino_cnpj.
    delete adjacent duplicates from lt_nfe_dist_aux comparing destino_cnpj.

    lr_destino_cnpj = value #( for ls_nfe_dist in lt_nfe_dist_aux (
                                                  sign = 'I'
                                                  option = 'EQ'
                                                  low    = ls_nfe_dist-destino_cnpj ) ).
    if lr_destino_cnpj is not initial.

      select branch,stcd1,bukrs
        from j_1bbranch
        into table @data(lt_branch)
        where stcd1  in @lr_destino_cnpj
        and   branch ne '0001'.
      if sy-subrc is initial.
        sort lt_branch by stcd1.

        data(lt_branch_aux) = lt_branch.
        sort lt_branch_aux by branch.
        delete adjacent duplicates from lt_branch_aux comparing branch.

        select *
          from zfis_filial_conf
          into table @data(lt_filial_conf)
          for all entries in @lt_branch_aux
          where filial     = @lt_branch_aux-branch.
        if sy-subrc is initial.
          sort lt_filial_conf by filial tipo_nota.
        endif.
      endif.

    endif.

    lt_nfe_dist_aux = lt_nfe_dist.

    sort lt_nfe_dist_aux by docnum_nfe docnum.
    delete adjacent duplicates from lt_nfe_dist_aux comparing docnum_nfe docnum.
    select docnum,nftype
      from j_1bnfdoc
      into table @data(lt_j_1bnfdoc)
      for all entries in @lt_nfe_dist_aux
      where ( docnum = @lt_nfe_dist_aux-docnum_nfe
         or   docnum = @lt_nfe_dist_aux-docnum )
        and ( nftype in @lr_notas_d or
              nftype in @lr_notas_s ).
    if sy-subrc is initial.
      sort lt_j_1bnfdoc by docnum.
    endif.

    lt_nfe_dist_aux = lt_nfe_dist.

    sort lt_nfe_dist_aux by chave_nfe.
    delete adjacent duplicates from lt_nfe_dist_aux comparing chave_nfe.

    select *
      from zib_nfe_forn
      into table @data(lt_nfe_forn)
      for all entries in @lt_nfe_dist_aux
      where nu_chave = @lt_nfe_dist_aux-chave_nfe.
    if sy-subrc is initial.
      sort lt_nfe_forn by nu_chave.
    endif.

  endif.

  loop at lt_nfe_dist assigning field-symbol(<fs_nfe_dist>).

    read table lt_j_1bnfdoc assigning  field-symbol(<fs_j_1bnfdoc>)
    with key docnum = <fs_nfe_dist>-docnum
    binary search.
    if sy-subrc is initial.
      if <fs_j_1bnfdoc>-nftype in lr_notas_d.
        data(lv_var_tipo) = '1'.
      elseif <fs_j_1bnfdoc>-nftype in lr_notas_s.
        lv_var_tipo = '2'.
      else.
        continue.
      endif.

      read table lt_branch assigning field-symbol(<fs_branch>)
      with key stcd1 = <fs_nfe_dist>-destino_cnpj
      binary search.
      if sy-subrc is initial.
        read table lt_filial_conf assigning field-symbol(<fs_filial_conf>)
        with key filial    = <fs_branch>-branch
                 tipo_nota = lv_var_tipo
        binary search.
        if sy-subrc is  initial.
          lv_hora_lim = sy-uzeit + 30.
          lv_data_par = sy-datum - <fs_filial_conf>-dias_corte.
          if <fs_nfe_dist>-dt_emissao < lv_data_par or
             ( <fs_filial_conf>-flag_apos_registro <> 'X' and
             not ( sy-uzeit BETWEEN <fs_filial_conf>-hora_fix_conf and lv_hora_lim  ) ).
            continue.
          endif.

          read table lt_nfe_forn assigning field-symbol(<fs_nfe_forn>)
          with key nu_chave = <fs_nfe_dist>-chave_nfe
          binary search.
          if sy-subrc is initial.
            if <fs_nfe_forn>-stop_env_man is not initial.
              continue.
            else.
              append initial line to lt_proc assigning field-symbol(<fs_proc>).
              <fs_proc>-chave     = <fs_nfe_dist>-chave_nfe.
              <fs_proc>-dest_cnpj = <fs_nfe_dist>-destino_cnpj.
              <fs_proc>-dest_ie   = <fs_nfe_dist>-destino_ie.
              <fs_proc>-bukrs     = <fs_branch>-bukrs.
              <fs_proc>-branch    = <fs_branch>-branch.
            endif.
          endif.
        else.
          continue.
        endif.

      else.
        continue.
      endif.

    else.
      continue.
    endif.

  endloop.

  if lt_proc is not initial.
    loop at lt_proc assigning <fs_proc>.

      free: lo_manifesto_dest.
      create object lo_manifesto_dest.

      lo_manifesto_dest->set_chave( <fs_proc>-chave ).
      lo_manifesto_dest->set_cd_operacao( '210200' ).
      lo_manifesto_dest->set_cnpj_dest( <fs_proc>-dest_cnpj ).
      lo_manifesto_dest->set_ie_dest( <fs_proc>-dest_ie ).
      lo_manifesto_dest->set_bukrs( <fs_proc>-bukrs ).
      lo_manifesto_dest->set_branch( <fs_proc>-branch ).

      try.

          read table lt_nfe_forn assigning <fs_nfe_forn>
          with key nu_chave = <fs_proc>-chave
          binary search.
          if sy-subrc is initial.

            <fs_nfe_forn>-manifesto_env = abap_true.
            add 1 to <fs_nfe_forn>-qtde_env_man.
            if  <fs_nfe_forn>-qtde_env_man >= 3.
              <fs_nfe_forn>-stop_env_man = abap_true.
            endif.
            modify zib_nfe_forn from <fs_nfe_forn>.
            commit work.

            lo_manifesto_dest->gravar_manifesto( receiving e_doc_manifesto = data(vl_doc_manifesto) ).
            if ( vl_doc_manifesto is not initial ).
              try.
                  lo_manifesto_dest->enviar_manifesto( i_sem_confirmacao =  'X' ).
                catch zcx_manifesto_dest into data(ex_man_dest).
                  ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
              endtry.

              <fs_nfe_forn>-doc_manifesto = vl_doc_manifesto.
              modify zib_nfe_forn from <fs_nfe_forn>.
              commit work.
            endif.

          endif.

        catch zcx_manifesto_dest into ex_man_dest.
          ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      endtry.

    endloop.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form f_filial_descon
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_filial_descon .

  types: begin of ty_proc,
           chave      type zib_nfe_forn-nu_chave,
           dt_emissao type zib_nfe_dist_ter-dt_emissao,
           dest_cnpj  type stcd1,
           dest_ie    type stcd3,
           bukrs      type zib_nfe_forn-bukrs,
           branch     type zib_nfe_forn-branch,
         end of ty_proc.


  data: lr_destino_cnpj   type range of j_1bbranch-stcd1,
        lr_notas_d        type range of j_1bnfdoc-nftype,
        lr_notas_s        type range of j_1bnfdoc-nftype,
        lv_data_par       type sy-datum,
        lv_dias_emissao   type i,
        lv_hora_lim       type sy-uzeit,
        lt_proc           type table of ty_proc,
        lo_manifesto_dest type ref to zcl_manifesto_dest,
        lv_dt_emissao     type sy-datum.


  lv_dt_emissao = sy-datum - 30.

  select a~model,a~dt_emissao,a~belnr,a~docnum_nfe,a~destino_cnpj,a~chave_nfe,a~destino_ie
    from zib_nfe_dist_ter as a
     inner join zib_nfe_forn as b
      on b~nu_chave = a~chave_nfe
     and ( b~docnum = '' or b~docnum = 0 )
    into table @data(lt_nfe_dist)
    where a~model         = '55'
      and a~dt_emissao    > @lv_dt_emissao
      and a~docnum_nfe = @abap_false
      and  not exists ( select *
                          from zsdt0127 as b
                          where a~chave_nfe = b~chave
                          and b~autorizado = @abap_true ).
  if sy-subrc is initial.

    delete adjacent duplicates from lt_nfe_dist comparing all fields.

    data(lt_nfe_dist_aux) = lt_nfe_dist.
    sort lt_nfe_dist_aux by destino_cnpj.
    delete adjacent duplicates from lt_nfe_dist_aux comparing destino_cnpj.

    lr_destino_cnpj = value #( for ls_nfe_dist in lt_nfe_dist_aux (
                                                  sign = 'I'
                                                  option = 'EQ'
                                                  low    = ls_nfe_dist-destino_cnpj ) ).
    if lr_destino_cnpj is not initial.

      select branch,stcd1,bukrs
        from j_1bbranch
        into table @data(lt_branch)
        where stcd1 in @lr_destino_cnpj
        and   branch ne '0001'.
      if sy-subrc is initial.
        sort lt_branch by stcd1.

        data(lt_branch_aux) = lt_branch.
        sort lt_branch_aux by branch.
        delete adjacent duplicates from lt_branch_aux comparing branch.

        select *
          from zfis_filial_desc
          into table @data(lt_filial_desc)
          for all entries in @lt_branch_aux
          where branch     = @lt_branch_aux-branch.
        if sy-subrc is initial.
          sort lt_filial_desc by branch.
        endif.

      endif.

    endif.

    lt_nfe_dist_aux = lt_nfe_dist.

    sort lt_nfe_dist_aux by chave_nfe.
    delete adjacent duplicates from lt_nfe_dist_aux comparing chave_nfe.

    select *
      from zib_nfe_forn
      into table @data(lt_nfe_forn)
      for all entries in @lt_nfe_dist_aux
      where nu_chave = @lt_nfe_dist_aux-chave_nfe.
    if sy-subrc is initial.
      sort lt_nfe_forn by nu_chave.
    endif.

  endif.

  loop at lt_nfe_dist assigning field-symbol(<fs_nfe_dist>).

    read table lt_branch assigning field-symbol(<fs_branch>)
    with key stcd1 = <fs_nfe_dist>-destino_cnpj
    binary search.
    if sy-subrc is initial.
      read table lt_filial_desc assigning field-symbol(<fs_filial_desc>)
      with key branch    = <fs_branch>-branch
      binary search.
      if sy-subrc is  initial.

        if <fs_filial_desc>-data_fixa is not initial.
          lv_data_par = <fs_filial_desc>-data_fixa.
        else.
          lv_data_par = sy-datum - <fs_filial_desc>-dias_descon.
        endif.

        lv_dias_emissao = sy-datum - <fs_nfe_dist>-dt_emissao.

        if lv_dias_emissao lt <fs_filial_desc>-dias_descon.
          continue.
        endif.

*        IF <fs_nfe_dist>-dt_emissao > lv_data_par.
*          CONTINUE.
*        ENDIF.

        read table lt_nfe_forn assigning field-symbol(<fs_nfe_forn>)
        with key nu_chave = <fs_nfe_dist>-chave_nfe
        binary search.
        if sy-subrc is initial.
          if <fs_nfe_forn>-stop_env_man is not initial.
            continue.
          else.
            append initial line to lt_proc assigning field-symbol(<fs_proc>).
            <fs_proc>-chave     = <fs_nfe_dist>-chave_nfe.
            <fs_proc>-dest_cnpj = <fs_nfe_dist>-destino_cnpj.
            <fs_proc>-dest_ie   = <fs_nfe_dist>-destino_ie.
            <fs_proc>-bukrs     = <fs_branch>-bukrs.
            <fs_proc>-branch    = <fs_branch>-branch.
          endif.
        endif.
      else.
        continue.
      endif.

    else.
      continue.
    endif.

  endloop.

  if lt_proc is not initial.

    loop at lt_proc assigning <fs_proc>.

      free: lo_manifesto_dest.
      create object lo_manifesto_dest.

      lo_manifesto_dest->set_chave( <fs_proc>-chave ).
      lo_manifesto_dest->set_cd_operacao( '210220' ).
      lo_manifesto_dest->set_cnpj_dest( <fs_proc>-dest_cnpj ).
      lo_manifesto_dest->set_ie_dest( <fs_proc>-dest_ie ).
      lo_manifesto_dest->set_bukrs( <fs_proc>-bukrs ).
      lo_manifesto_dest->set_branch( <fs_proc>-branch ).

      try.

          read table lt_nfe_forn assigning <fs_nfe_forn>
          with key nu_chave = <fs_proc>-chave
          binary search.
          if sy-subrc is initial.
            <fs_nfe_forn>-manifesto_env = abap_true.
            add 1 to <fs_nfe_forn>-qtde_env_man.
            if  <fs_nfe_forn>-qtde_env_man >= 3.
              <fs_nfe_forn>-stop_env_man = abap_true.
            endif.
            modify zib_nfe_forn from <fs_nfe_forn>.
            commit work.

            lo_manifesto_dest->gravar_manifesto( receiving e_doc_manifesto = data(vl_doc_manifesto) ).
            if ( vl_doc_manifesto is not initial ).

              try.
                  lo_manifesto_dest->enviar_manifesto( i_sem_confirmacao =  'X' ).
                catch zcx_manifesto_dest into data(ex_man_dest).
                  ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
              endtry.

              <fs_nfe_forn>-doc_manifesto = vl_doc_manifesto.
              modify zib_nfe_forn from <fs_nfe_forn>.
              commit work.
            endif.

          endif.

        catch zcx_manifesto_dest into ex_man_dest.
          ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      endtry.

    endloop.

  endif.
endform.
*&---------------------------------------------------------------------*
*& Form f_envia_email
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_envia_email .

  data: lr_destino_cnpj   type range of j_1bbranch-stcd1,
        lr_notas_d        type range of j_1bnfdoc-nftype,
        lr_notas_s        type range of j_1bnfdoc-nftype,
        lv_data_par       type sy-datum,
        lv_hora_lim       type sy-uzeit,
        lv_dias_emissao   type i,
        lt_proc           type table of ty_proc_email,
        lo_manifesto_dest type ref to zcl_manifesto_dest,
        lv_dt_emissao     type sy-datum,
        lt_html           type table of w3html,
        lt_packing_list   type table of sopcklsti1,
        ls_document_data  type sodocchgi1,
        lt_receivers      type table of somlreci1.


  lv_dt_emissao = sy-datum - 60.

  select a~model,
         a~dt_emissao,
         a~belnr,
         a~docnum_nfe,
         a~destino_cnpj,
         a~chave_nfe,
         a~destino_ie,
         a~forne_cnpj,
         a~forne_razao,
         a~vl_total_fatura
    from zib_nfe_dist_ter as a
    inner join zib_nfe_forn as b
      on b~nu_chave = a~chave_nfe
     and ( b~docnum = '' or b~docnum = 0 )
    into table @data(lt_nfe_dist)
    where a~model         = '55'
      and a~dt_emissao    > @lv_dt_emissao
       and  not exists ( select *  from
                          zsdt0127 as b
                          where a~chave_nfe = b~chave
                          and b~autorizado = @abap_true ).
  if sy-subrc is initial.
    delete adjacent duplicates from lt_nfe_dist comparing all fields.

    data(lt_nfe_dist_aux) = lt_nfe_dist.
    sort lt_nfe_dist_aux by destino_cnpj.
    delete adjacent duplicates from lt_nfe_dist_aux comparing destino_cnpj.

    lr_destino_cnpj = value #( for ls_nfe_dist in lt_nfe_dist_aux (
                                                  sign = 'I'
                                                  option = 'EQ'
                                                  low    = ls_nfe_dist-destino_cnpj ) ).
    if lr_destino_cnpj is not initial.

      select branch,stcd1,bukrs
        from j_1bbranch
        into table @data(lt_branch)
        where stcd1 in @lr_destino_cnpj
        and   branch ne '0001'.
      if sy-subrc is initial.
        sort lt_branch by stcd1.

        data(lt_branch_aux) = lt_branch.
        sort lt_branch_aux by branch.
        delete adjacent duplicates from lt_branch_aux comparing branch.

        select *
          from zfis_filial_desc
          into table @data(lt_filial_desc)
          for all entries in @lt_branch_aux
          where branch     = @lt_branch_aux-branch.
        if sy-subrc is initial.
          sort lt_filial_desc by branch.
        endif.

        lt_branch_aux = lt_branch.
        sort lt_branch_aux by bukrs branch.
        delete adjacent duplicates from lt_branch_aux comparing bukrs branch.

        select *
          from zmail
          into table @data(lt_email)
          for all entries in @lt_branch_aux
          where tcode = 'ZSDT0110'
            and bukrs = @lt_branch_aux-bukrs
            and werks = @lt_branch_aux-branch.
        if sy-subrc is not initial.

          select *
            from zmail
            into table lt_email
            for all entries in lt_branch_aux
            where tcode = 'ZSDT0110'
              and werks = lt_branch_aux-branch.
          if sy-subrc is not initial.

            select *
              from zmail
              into table lt_email
              for all entries in lt_branch_aux
              where tcode = 'ZSDT0110'
                and bukrs = lt_branch_aux-bukrs.

          endif.

        endif.

        sort lt_email by bukrs werks.

      endif.

    endif.

  endif.

  data(lt_email_branch) = lt_email.
  sort lt_email_branch by werks.

  data(lt_email_bukrs) = lt_email.
  sort lt_email_bukrs by bukrs.

  loop at lt_nfe_dist assigning field-symbol(<fs_nfe_dist>).

    read table lt_branch assigning field-symbol(<fs_branch>)
    with key stcd1 = <fs_nfe_dist>-destino_cnpj
    binary search.
    if sy-subrc is initial.

      read table lt_email assigning field-symbol(<fs_email>)
      with key bukrs  = <fs_branch>-bukrs
               werks = <fs_branch>-branch
      binary search.
      if sy-subrc is not initial.
        read table lt_email_branch assigning <fs_email>
        with key werks = <fs_branch>-branch
        binary search.
        if sy-subrc is not initial.

          read table lt_email_bukrs assigning <fs_email>
          with key bukrs = <fs_branch>-bukrs
          binary search.
          if sy-subrc is not initial.
            continue.
          endif.

        endif.

      endif.

      read table lt_filial_desc assigning field-symbol(<fs_filial_desc>)
      with key branch    = <fs_branch>-branch
      binary search.
      if sy-subrc is  initial.

        lv_data_par = sy-datum - <fs_filial_desc>-dias_envio_email.

        IF <fs_filial_desc>-dias_envio_email = 0.
            CONTINUE. "ALRS
        ENDIF.

        lv_dias_emissao = sy-datum - <fs_nfe_dist>-dt_emissao.

        if lv_dias_emissao lt <fs_filial_desc>-dias_envio_email.
          continue.
        else.

          append initial line to lt_proc assigning field-symbol(<fs_proc>).
          <fs_proc>-chave_nfe   = <fs_nfe_dist>-chave_nfe.
          <fs_proc>-dias_recusa = lv_data_par - <fs_nfe_dist>-dt_emissao.
          <fs_proc>-forne_cnpj  = <fs_nfe_dist>-forne_cnpj.
          <fs_proc>-forne_razao = <fs_nfe_dist>-forne_razao.
          <fs_proc>-dt_emissao  = <fs_nfe_dist>-dt_emissao.
          <fs_proc>-vl_fatura   = <fs_nfe_dist>-vl_total_fatura.

        endif.

      else.
        continue.
      endif.

    else.
      continue.
    endif.

  endloop.

  if lt_proc is not initial.
    perform monta_cabecalho tables lt_html.
    loop at lt_proc assigning <fs_proc>.
      perform monta_detalhe tables lt_html
                            using  <fs_proc>.
    endloop.
    perform monta_fim tables lt_html lt_packing_list changing ls_document_data.

    loop at lt_email assigning <fs_email>.

      append initial line to lt_receivers assigning field-symbol(<fs_receivers>).
      <fs_receivers>-receiver = <fs_email>-email.
      <fs_receivers>-rec_type = 'U'.
      <fs_receivers>-express  = 'X'.

      if <fs_email>-usuario is not initial.
        append initial line to lt_receivers assigning <fs_receivers>.
        <fs_receivers>-receiver = <fs_email>-usuario.
        <fs_receivers>-rec_type = 'B'.
        <fs_receivers>-express  = 'X'.
      endif.


    endloop.

    call function 'ZHTML_ENVIA_EMAIL_CP'
      exporting
        i_titulo      = 'Nf-e Pendentes de registro no SAP'
        document_data = ls_document_data
      tables
        packing_list  = lt_packing_list
        html          = lt_html
        receivers     = lt_receivers.

    message 'Email enviado com sucesso!' type 'S'.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form MONTA_CABECALHO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_HTML
*&---------------------------------------------------------------------*
form monta_cabecalho  tables  p_html structure w3html.

  data: vg_html           type string.

  "Início cabeçalho
  perform add tables p_html using '<tr><td>'.
  perform add tables p_html using '<BR>'.
  perform add tables p_html using '<DIV align=center><FONT face=Arial color=#ff0000 size=4><STRONG>Nf-e Pendentes de registro no SAP</STRONG></FONT></DIV>'.
  perform add tables p_html using '<DIV align=left>'.
  perform add tables p_html using '<FONT face=Arial color=#0000ff size=2>'.

  "Início da Tabela de detalhe
  perform add tables p_html using '<table align="left" cellspacing="0" cellpadding="9" border="1">'.
  concatenate '<tr><td colspan="9" align="center" bgcolor="666666"><font color="#FFFFFF" size=2><strong>'
              'Se não for providenciada a entrada dessas notas, elas irão ser recusadas'
              '</strong></font></td></tr>'
         into vg_html.
  perform add tables p_html using vg_html.
  concatenate '<tr>'
              '<td><p align="right"><font color="#000030" size=2><b>Chave NFE</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Dias Recusa</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Fornecedor CNPJ</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Fornecedor Razão</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Data Emissão</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Valor Total Fatura</b></font></p></td>'
              '</tr>' into vg_html.
  perform add tables p_html using vg_html.
endform.
*&---------------------------------------------------------------------*
*& Form add
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_HTML
*&      --> P_
*&---------------------------------------------------------------------*
form add  tables   p_html  structure w3html
          using    p_texto type string.

  call function 'ZHTML_ADD'
    exporting
      i_texto = p_texto
    tables
      it_html = p_html.

endform.
*&---------------------------------------------------------------------*
*& Form monta_detalhe
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PROC
*&---------------------------------------------------------------------*
form monta_detalhe  tables p_html structure w3html
                    using p_proc type ty_proc_email.

  data: vg_html      type string,
        xmodelo      type c length 02,
        xnumero      type c length 09,
        xserie       type c length 03,
        lv_vl_fatura type string.

*  WRITE: wa_j_1bnfdoc-model  TO xmodelo,
*         wa_j_1bnfdoc-nfenum TO xnumero,
*         wa_j_1bnfdoc-series TO xserie .

*  SHIFT:  xmodelo LEFT DELETING LEADING space,
*          xnumero LEFT DELETING LEADING space,
*          xserie  LEFT DELETING LEADING space.

  lv_vl_fatura = p_proc-vl_fatura.

  concatenate '<tr>'
              '<td><font color="#000000" size=2>' p_proc-chave_nfe '</font></td>'
              '<td><font color="#000000" size=2>' p_proc-dias_recusa '</font></td>'
              '<td><font color="#000000" size=2>' p_proc-forne_cnpj  '</font></td>'
              '<td><font color="#000000" size=2>' p_proc-forne_razao '</font></td>'
              '<td><font color="#000000" size=2>' p_proc-dt_emissao+6(2) '/' p_proc-dt_emissao+4(2) '/' p_proc-dt_emissao(4) '</font></td>'
              '<td><font color="#000000" size=2>' lv_vl_fatura '</font></td>'
              '</tr>' into vg_html.
  perform add tables p_html using vg_html.

endform.

form monta_fim  tables   p_html       structure w3html
                         packing_list structure sopcklsti1
                changing document_data type sodocchgi1.

  "Fim da Tabela de detalhe
  perform add tables p_html using '</table>'.
  perform add tables p_html using '</DIV>'.
  perform add tables p_html using '</td></tr>'.

  "Fim cabeçalho
  perform add tables p_html using '</DIV>'.
  perform add tables p_html using '</td></tr>'.

  document_data-obj_name  = 'ProcArqC'.
  document_data-obj_descr = 'NF-e pendentes de registro no SAP'.
  document_data-no_change = 'X'.

  packing_list-head_start = 1.
  packing_list-head_num   = 60.
  packing_list-body_start = 1.
  packing_list-body_num   = 99999.
  packing_list-doc_type   = 'HTM'.
  append packing_list.


endform.                    " MONTA_FIM
*&---------------------------------------------------------------------*
*& Form f_envia_email_fiscal
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_envia_email_fiscal .

  data: lv_dt_param      type sy-datum,
        lt_html          type table of w3html,
        lt_packing_list  type table of sopcklsti1,
        ls_document_data type sodocchgi1,
        lt_receivers     type table of somlreci1.

  lv_dt_param = sy-datum - 5.

  select a~company_code,
         a~supplier_cnpj_cpf,
         a~nfenum,
         a~series,
         a~issue_date,
         a~plant,
         a~total_value,
         a~accesskey
    from edobrincoming as a
    into table @data(lt_coming)
    where issue_date > @lv_dt_param
      and md_final_status = @space.
  if sy-subrc is initial.
    data(lt_coming_aux) = lt_coming.
    sort lt_coming_aux by accesskey.
    delete adjacent duplicates from lt_coming_aux comparing accesskey.

    select *
      from  zib_nfe_dist_ter
      into table @data(lt_dist_ter)
      for all entries in @lt_coming_aux
      where chave_nfe = @lt_coming_aux-accesskey.
    if sy-subrc is initial.
      sort lt_dist_ter by chave_nfe.

      loop at lt_coming assigning field-symbol(<fs_coming>).
        data(lv_tabix) = sy-tabix.

        read table lt_dist_ter transporting no fields
        with key chave_nfe = <fs_coming>-accesskey
        binary search.
        if sy-subrc is initial.
          delete lt_coming index lv_tabix.
        endif.
      endloop.
    endif.

    perform monta_cabecalho_fiscal tables lt_html.
    loop at lt_coming assigning <fs_coming>.
      perform monta_detalhe_fiscal tables lt_html
                            using  <fs_coming>.
    endloop.
    perform monta_fim_fiscal tables lt_html lt_packing_list changing ls_document_data.


    append initial line to lt_receivers assigning field-symbol(<fs_receivers>).
    <fs_receivers>-receiver = 'CSC.FISCAL.SUPORTE@AMAGGI.COM.BR'.
    <fs_receivers>-rec_type = 'U'.
    <fs_receivers>-express  = 'X'.

    call function 'ZHTML_ENVIA_EMAIL_CP'
      exporting
        i_titulo      = 'Nf-e Pendentes de registro no SAP'
        document_data = ls_document_data
      tables
        packing_list  = lt_packing_list
        html          = lt_html
        receivers     = lt_receivers.

    message 'Email enviado com sucesso!' type 'S'.

  endif.


endform.

*&---------------------------------------------------------------------*
*& Form MONTA_CABECALHO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_HTML
*&---------------------------------------------------------------------*
form monta_cabecalho_fiscal  tables  p_html structure w3html.

  data: vg_html           type string.

  "Início cabeçalho
  perform add tables p_html using '<tr><td>'.
  perform add tables p_html using '<BR>'.
  perform add tables p_html using '<DIV align=center><FONT face=Arial color=#ff0000 size=4><STRONG>NF-e não distribuidas</STRONG></FONT></DIV>'.
  perform add tables p_html using '<DIV align=left>'.
  perform add tables p_html using '<FONT face=Arial color=#0000ff size=2>'.

  "Início da Tabela de detalhe
  perform add tables p_html using '<table align="left" cellspacing="0" cellpadding="9" border="1">'.
  concatenate '<tr><td colspan="9" align="center" bgcolor="666666"><font color="#FFFFFF" size=2><strong>'
              'Nf-e Pendentes de registro no SAP'
              '</strong></font></td></tr>'
         into vg_html.
  perform add tables p_html using vg_html.
  concatenate '<tr>'
*              '<td><font color="#000030" size=2><b>Docnum</b></font></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Empresa</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>CNPJ_CPF</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Numero/Serie</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Data Emissão</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Centro</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Valor</b></font></p></td>'
*              '<td><font color="#000030" size=2><b>Dt. Emissão</b></font></td>'
*              '<td><font color="#000030" size=2><b>Usuário</b></font></td>'
*              '<td><font color="#000030" size=2><b>Etapa Necessária</b></font></td>'
              '</tr>' into vg_html.
  perform add tables p_html using vg_html.
endform.

*&---------------------------------------------------------------------*
*& Form monta_detalhe
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PROC
*&---------------------------------------------------------------------*
form monta_detalhe_fiscal tables p_html structure w3html
                    using p_proc type ty_proc_email_fiscal.

  data: vg_html         type string,
        xmodelo         type c length 02,
        xnumero         type c length 09,
        xserie          type c length 03,
        lv_vl_fatura    type string,
        lv_numero_serie type string.

*  WRITE: wa_j_1bnfdoc-model  TO xmodelo,
*         wa_j_1bnfdoc-nfenum TO xnumero,
*         wa_j_1bnfdoc-series TO xserie .

*  SHIFT:  xmodelo LEFT DELETING LEADING space,
*          xnumero LEFT DELETING LEADING space,
*          xserie  LEFT DELETING LEADING space.

  lv_vl_fatura = p_proc-total_value.

  if p_proc-nfenum is not initial.
    lv_numero_serie = p_proc-nfenum.
  elseif p_proc-series is not initial.
    lv_numero_serie = p_proc-series.
  endif.

  concatenate '<tr>'
              '<td><font color="#000000" size=2>' p_proc-empresa '</font></td>'
              '<td><font color="#000000" size=2>' p_proc-cnpj_cpf  '</font></td>'
              '<td><font color="#000000" size=2>' lv_numero_serie '</font></td>'
*              '<td><p align="right"><font color="#000000" size=2>' xmodelo '</font></p></td>'
*              '<td><p align="right"><font color="#000000" size=2>' xnumero '</font></p></td>'
*              '<td><p align="right"><font color="#000000" size=2>' xserie  '</font></p></td>'
              '<td><font color="#000000" size=2>' p_proc-issue_date+6(2) '/' p_proc-issue_date+4(2) '/' p_proc-issue_date(4) '</font></td>'
              '<td><font color="#000000" size=2>' p_proc-plant '</font></td>'
              '<td><font color="#000000" size=2>' lv_vl_fatura '</font></td>'
*              '<td><font color="#000000" size=2>' wa_dmo_text-ddtext '</font></td>'
              '</tr>' into vg_html.
  perform add tables p_html using vg_html.

endform.

form monta_fim_fiscal  tables   p_html       structure w3html
                         packing_list structure sopcklsti1
                changing document_data type sodocchgi1.

  "Fim da Tabela de detalhe
  perform add tables p_html using '</table>'.
  perform add tables p_html using '</DIV>'.
  perform add tables p_html using '</td></tr>'.

  "Fim cabeçalho
  perform add tables p_html using '</DIV>'.
  perform add tables p_html using '</td></tr>'.

  document_data-obj_name  = 'ProcArqC'.
  document_data-obj_descr = 'NF-e não distribuidas'.
  document_data-no_change = 'X'.

  packing_list-head_start = 1.
  packing_list-head_num   = 60.
  packing_list-body_start = 1.
  packing_list-body_num   = 99999.
  packing_list-doc_type   = 'HTM'.
  append packing_list.


endform.                    " MONTA_FIM
