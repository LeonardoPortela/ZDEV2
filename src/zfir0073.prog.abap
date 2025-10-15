*&---------------------------------------------------------------------*
*& Report  ZFIR0073
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zfir0073.

* [CS2016000946]>>>>
types: begin of ty_saida,
         augbl type bsad-augbl,
         awtyp type bkpf-awtyp,
         awref type bkpf-awkey,
         buzei type bsid_view-buzei,
       end of ty_saida.
* [CS2016000946]<<<<

data: wa_zfit0045 type zfit0045,
      it_zfit0045 type table of zfit0045,
      gt_accchg   type table of accchg,
      gs_accchg   type accchg,

      it_bsak_ad  type table of bsak,
      wa_bsak_ad  type bsak,
      it_bsik_au  type table of bsik,
      wa_bsik_au  type bsik,
      wa_bkpf_au  type bkpf.

* [CS2016000946]>>>>
data: tw_zsdt0054 type table of zsdt0054 with header line,
      tw_zsdt0051 type table of zsdt0051 with header line,
      tw_bsad     type table of bsad     with header line,
      tw_bsid     type table of bsid     with header line,
      tw_bkpf     type table of bkpf     with header line,
      t_adiant    type range of zsdt0054-adiant,
      l_adiant    like line of t_adiant,
      tw_saida    type table of ty_saida with header line.
* [CS2016000946]<<<<



*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
start-of-selection.
  data: vg_job      type i.
*S: Scheduled (Agendado/Programado)
*R: Released (Liberado)
*Y: Ready (Pronto na fila)
*A: Active (Em execução)
*F: Finished (Finalizado com sucesso)
*C: Cancelled (Cancelado)

  select single count( * ) into vg_job
    from tbtco
   where jobname eq 'AGING_ADTO'
     and status eq 'R'. "IR236265 - JOB executando mas não altera na tabela - #178826 - BG ---- Alterado de R para S  VOLTEI PARA R ALRS 06/10/2025

  if ( vg_job eq 1 ).
    perform f_seleciona_dados.
  endif.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_seleciona_dados .


  data: lt_bkdf  type table of bkdf,
        lt_bkpf  type table of bkpf,
        lt_bsec  type table of bsec,
        wa_bseg  type bseg,
        lt_bsed  type table of bsed,
        lt_bseg  type table of bseg,
        lt_bset  type table of bset,
        p_valor  type bseg-dmbtr,
        p_xref1  type bseg-xref1,
        p_xref3  type bseg-xref3,
        p_bseg   type bseg,
        p_lote   type zfit0156-lote,
        msg_text type string,
        lv_achou type c.

  select *
    from zfit0045
    into table it_zfit0045
    where atual_dt_liq = ''.

  loop at it_zfit0045 into wa_zfit0045.
    select  *
      from bsak
      where bukrs  eq @wa_zfit0045-bukrs
      and   belnr  eq @wa_zfit0045-belnr
      and   lifnr  eq @wa_zfit0045-lifnr
      and   augbl  ne ''
      into table @it_bsak_ad.

    loop at it_bsak_ad into wa_bsak_ad.
      refresh it_bsik_au.
      select  *
         from bsik
         into table it_bsik_au
         where bukrs  eq wa_bsak_ad-bukrs
         and   belnr  eq wa_bsak_ad-augbl
         and   lifnr  eq wa_bsak_ad-lifnr.
      loop at it_bsik_au into wa_bsik_au.
        select single *
            from bkpf
            into wa_bkpf_au
            where bukrs  eq wa_bsik_au-bukrs
            and   belnr  eq wa_bsik_au-belnr
            and   gjahr  eq wa_bsik_au-gjahr.

*        REFRESH gt_accchg.
*        gs_accchg-fdname = 'ZFBDT'.
*        gs_accchg-newval =  wa_zfit0045-dt_prev_liq.
*        APPEND gs_accchg TO gt_accchg.
*        CLEAR gs_accchg.

*        CALL FUNCTION 'FI_DOCUMENT_CHANGE'
*          EXPORTING
*            i_awtyp              = wa_bkpf_au-awtyp
*            i_awref              = wa_bkpf_au-awkey+00(10)
*            i_aworg              = wa_bkpf_au-awkey+10(10)
*            i_buzei              = wa_bsik_au-buzei
*          TABLES
*            t_accchg             = gt_accchg
*          EXCEPTIONS
*            no_reference         = 1
*            no_document          = 2
*            many_documents       = 3
*            wrong_input          = 4
*            overwrite_creditcard = 5
*            OTHERS               = 6.

        "PSA ALINHADO COM ANTONIO

        append wa_bkpf_au to lt_bkpf.
        clear wa_bseg.
        refresh  lt_bseg.

** //===========================|| IR236265 / AOENNING / 25-08-2025 ||===================================
        select single *
         from bseg
         into  wa_bseg
          where bukrs = wa_bkpf_au-bukrs
          and   belnr = wa_bkpf_au-belnr
          and   gjahr = wa_bkpf_au-gjahr
          and   bschl in ( '29' ).

*        MOVE-CORRESPONDING p_bseg TO wa_bseg. "Comentado e adicionado processo seguinte.
        if sy-subrc eq  0.
          wa_bseg-xref1 = p_xref1.
          wa_bseg-xref3 = p_xref3.
          wa_bseg-zfbdt = wa_zfit0045-dt_prev_liq.
          append wa_bseg to lt_bseg.
        endif.
** //===========================//===================================

        call function 'CHANGE_DOCUMENT'
          tables
            t_bkdf = lt_bkdf
            t_bkpf = lt_bkpf
            t_bsec = lt_bsec
            t_bsed = lt_bsed
            t_bseg = lt_bseg
            t_bset = lt_bset.

        if sy-subrc = 0.

          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          update zfit0045 set atual_dt_liq = 'X'
          where nro_sol = wa_zfit0045-nro_sol.
          commit work.
        endif.

      endloop.

    endloop.

  endloop.


* [CS2016000946]>>>>

  select * from zsdt0054
      into table tw_zsdt0054
        where atualizado ne abap_true
          and adiant ne abap_false.

  check not tw_zsdt0054[] is initial.

  select * from zsdt0051
    into table tw_zsdt0051
     for all entries in tw_zsdt0054
    where nro_sol_ov eq tw_zsdt0054-nro_sol_ov.

  loop at tw_zsdt0054.

    clear: l_adiant.

    l_adiant-sign   =  'I'.
    l_adiant-option = 'EQ'.
    l_adiant-low    = tw_zsdt0054-adiant.
    l_adiant-high   = tw_zsdt0054-adiant.
    append l_adiant to t_adiant.

  endloop.

  if not tw_zsdt0051[] is initial.

    select * from bsad
       into table tw_bsad
       for all entries in tw_zsdt0051
      where bukrs eq tw_zsdt0051-vkorg
        and kunnr eq tw_zsdt0051-kunnr
        and belnr in t_adiant
        and augbl ne abap_false.

    if not tw_bsad[] is initial.

      select * from bsid
        for all entries in @tw_bsad
        where bukrs eq @tw_bsad-bukrs
          and kunnr eq @tw_bsad-kunnr
          and belnr eq @tw_bsad-augbl
                into table @tw_bsid.

      if not tw_bsid[] is initial.

        select * from bkpf
          into table tw_bkpf
            for all entries in tw_bsid
          where bukrs eq tw_bsid-bukrs
            and belnr eq tw_bsid-belnr
            and gjahr eq tw_bsid-gjahr.

      endif.
    endif.
  endif.


  sort: tw_zsdt0054 by nro_sol_ov adiant,
        tw_zsdt0051 by nro_sol_ov,
        tw_bsad by bukrs kunnr augbl,
        tw_bsid by bukrs belnr gjahr.

  loop at tw_bkpf.

    read table tw_bsid with key bukrs = tw_bkpf-bukrs
                                belnr = tw_bkpf-belnr
                                gjahr = tw_bkpf-gjahr.
    if not sy-subrc is initial. continue. endif.

    read table tw_bsad with key bukrs = tw_bsid-bukrs
                                kunnr = tw_bsid-kunnr
                                augbl = tw_bsid-belnr.
    if not sy-subrc is initial. continue. endif.

    read table tw_zsdt0054 with key adiant = tw_bsad-belnr.
    if not sy-subrc is initial. continue. endif.

    read table tw_zsdt0051 with key nro_sol_ov = tw_zsdt0054-nro_sol_ov.
    if not sy-subrc is initial. continue. endif.

    refresh gt_accchg.
    gs_accchg-fdname = 'ZFBDT'.
    gs_accchg-newval =  tw_zsdt0051-dtate_logist.
    append gs_accchg to gt_accchg.
    clear gs_accchg.

    call function 'FI_DOCUMENT_CHANGE'
      exporting
        i_awtyp              = tw_bkpf-awtyp
        i_awref              = tw_bkpf-awkey+00(10)
        i_aworg              = tw_bkpf-awkey+10(10)
        i_buzei              = tw_bsid-buzei
      tables
        t_accchg             = gt_accchg
      exceptions
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        others               = 6.

    if sy-subrc is initial.

      update zsdt0054 set atualizado = abap_true
            where nro_sol_ov = tw_zsdt0054-nro_sol_ov
              and adiant = tw_bsad-belnr.

      commit work.

    endif.

  endloop.

*** Inicio - Rubenilson - 03.09.24 - US128351

  select bukrs,belnr,gjahr,vbel2,buzei
    from bsid
    into table @data(lt_bsid)
    where bschl = '19'
      and blart not in ('XR','VC')
      and vbel2 <> @space.

  if sy-subrc is initial.

    data(lt_bsid_aux) = lt_bsid.
    sort lt_bsid_aux by bukrs belnr gjahr.
    delete adjacent duplicates from lt_bsid_aux comparing bukrs belnr gjahr.

    select * from bkpf
      into table @data(lt_bkpf2)
        for all entries in @lt_bsid_aux
      where bukrs eq @lt_bsid_aux-bukrs
        and belnr eq @lt_bsid_aux-belnr
        and gjahr eq @lt_bsid_aux-gjahr.
    if sy-subrc is initial.
      sort lt_bkpf2 by bukrs belnr gjahr.
    endif.

    lt_bsid_aux = lt_bsid.
    sort lt_bsid_aux by vbel2.
    delete adjacent duplicates from lt_bsid_aux comparing vbel2.

    select vbeln,doc_simulacao,auart,spart "153746 CS2023000827 Job Alt.Venc.Adiant.Cli. - P3 - PSA
      from zsdt0041
      into table @data(lt_zsdt0041)
      for all entries in @lt_bsid_aux
      where vbeln = @lt_bsid_aux-vbel2
        and venc_editado = @abap_false.
    if sy-subrc is initial.

      sort lt_zsdt0041 by vbeln.
      data(lt_dados) = lt_zsdt0041.

    endif.

    select vbeln,doc_simulacao,auart,spart "153746 CS2023000827 Job Alt.Venc.Adiant.Cli. - P3 - PSA
      from zsdt0090
      into table @data(lt_zsdt0090)
      for all entries in @lt_bsid_aux
      where vbeln = @lt_bsid_aux-vbel2
        and venc_editado = @abap_false.
    if sy-subrc is initial.
      sort lt_zsdt0090 by vbeln.

      append lines of lt_zsdt0090 to lt_dados.
    endif.

    if lt_dados is not initial.

      sort lt_dados by vbeln.

      data(lt_dados_aux) = lt_dados.
      sort lt_dados_aux by doc_simulacao.
      delete adjacent duplicates from lt_dados_aux comparing doc_simulacao.

      select doc_simulacao,safra,cultura,kunnr,dt_entrega_sem,dt_entrega_def,dt_entrega_fet "153746 CS2023000827 Job Alt.Venc.Adiant.Cli. - P3 - PSA
        from zsdt0040
        into table @data(lt_zsdt0040)
        for all entries in @lt_dados_aux
        where doc_simulacao = @lt_dados_aux-doc_simulacao.
      if sy-subrc is initial.

        sort lt_zsdt0040 by doc_simulacao.

        data(lt_zsdt0040_aux) = lt_zsdt0040.
        sort lt_zsdt0040_aux by kunnr.
        delete adjacent duplicates from lt_zsdt0040_aux comparing kunnr.

        select kunnr,regio
          from kna1
          into table @data(lt_kna1)
          for all entries in @lt_zsdt0040_aux
          where kunnr = @lt_zsdt0040_aux-kunnr.
        if sy-subrc is initial.
          sort lt_kna1 by kunnr.
        endif.

        sort lt_zsdt0040_aux by safra cultura.
        delete adjacent duplicates from lt_zsdt0040_aux comparing safra cultura.

        select *
          from zsdt0347
          into table @data(lt_zsdt0347)
          for all entries in @lt_zsdt0040_aux
          where safra   = @lt_zsdt0040_aux-safra
            and cultura = @lt_zsdt0040_aux-cultura.
        if sy-subrc is initial.
          sort lt_zsdt0347 by safra cultura tipo_ov uf.
        endif.
      endif.
    endif.

    select *
      from zsdt0053
      into table @data(lt_zsdt0053)
      for all entries in @lt_bsid_aux
      where vbeln = @lt_bsid_aux-vbel2.
    if sy-subrc is initial.
      sort lt_zsdt0053 by vbeln.

      data(lt_zsdt0053_aux) = lt_zsdt0053.
      sort lt_zsdt0053_aux by nro_sol_ov.
      delete adjacent duplicates from lt_zsdt0053_aux comparing nro_sol_ov.

      select *
        from zsdt0051
        into table @data(lt_zsdt0051)
        for all entries in @lt_zsdt0053_aux
        where nro_sol_ov = @lt_zsdt0053_aux-nro_sol_ov
          and venc_editado = @abap_false.
      if sy-subrc is initial.
        sort lt_zsdt0051 by nro_sol_ov.
      endif.

    endif.
    data: _newval type accchg-newval.

    loop at lt_bsid assigning field-symbol(<fs_bsid>).

      read table lt_bkpf2 assigning field-symbol(<fs_bkpf>)
      with key bukrs = <fs_bsid>-bukrs
               belnr = <fs_bsid>-belnr
               gjahr = <fs_bsid>-gjahr
      binary search.

      if sy-subrc is initial.

        read table lt_zsdt0053 assigning field-symbol(<fs_zsdt0053>)
        with key vbeln = <fs_bsid>-vbel2
        binary search.

        if sy-subrc is initial.

          read table lt_zsdt0051 assigning field-symbol(<fs_zsdt0051>)
          with key nro_sol_ov = <fs_zsdt0053>-nro_sol_ov
          binary search.
          if sy-subrc is initial.

            lv_achou = abap_true.

            refresh gt_accchg.
            gs_accchg-fdname = 'ZFBDT'.
            gs_accchg-newval =  <fs_zsdt0051>-dtate_logist.
            append gs_accchg to gt_accchg.
            clear gs_accchg.

            call function 'FI_DOCUMENT_CHANGE'
              exporting
                i_awtyp              = <fs_bkpf>-awtyp
                i_awref              = <fs_bkpf>-awkey+00(10)
                i_aworg              = <fs_bkpf>-awkey+10(10)
                i_buzei              = <fs_bsid>-buzei
              tables
                t_accchg             = gt_accchg
              exceptions
                no_reference         = 1
                no_document          = 2
                many_documents       = 3
                wrong_input          = 4
                overwrite_creditcard = 5
                others               = 6.

            update zsdt0051 set venc_editado = abap_true where nro_sol_ov = <fs_zsdt0051>-nro_sol_ov.
            if sy-subrc is initial.
              commit work.
            endif.

          endif.

        endif.

        if lv_achou is initial.

          read table lt_dados assigning field-symbol(<fs_dados>)
          with key vbeln = <fs_bsid>-vbel2
          binary search.

          if sy-subrc is initial.

            read table lt_zsdt0040 assigning field-symbol(<fs_zsdt0040>)
            with key doc_simulacao = <fs_dados>-doc_simulacao
            binary search.
            if sy-subrc is initial.
              read table lt_kna1 assigning field-symbol(<fs_kna1>)
              with key kunnr = <fs_zsdt0040>-kunnr
              binary search.
              if sy-subrc is initial.
**********************************************************************107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
                types: begin of ty_tipo_ov,
                         auart type auart,
                       end of ty_tipo_ov.
                data: it_tipo_ov type standard table of ty_tipo_ov initial size 0.
                free: it_tipo_ov.

                data it_zsdt0347_aux type standard table of zsdt0347 initial size 0.
                append lines of lt_zsdt0347 to it_zsdt0347_aux.

                loop at it_zsdt0347_aux assigning field-symbol(<fs_rm_zsdt0347>).
                  if <fs_rm_zsdt0347>-tipo_ov is not initial.
                    split <fs_rm_zsdt0347>-tipo_ov at ',' into table it_tipo_ov.
                    delete it_tipo_ov where auart <> <fs_dados>-auart.
                  endif.
                  if it_tipo_ov is not initial
                  and <fs_rm_zsdt0347>-safra   = <fs_zsdt0040>-safra
                         and <fs_rm_zsdt0347>-cultura = <fs_zsdt0040>-cultura.
                  else.
                    delete it_zsdt0347_aux index sy-tabix.
                  endif.
                endloop.
**********************************************************************107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
                clear:_newval.
                if it_zsdt0347_aux is not initial.
                  "Verifica se tem regiao
                  read table it_zsdt0347_aux assigning field-symbol(<fs_get1_zsdt0347>) with key uf = <fs_kna1>-regio.
                  if sy-subrc = 0 .
                    _newval = <fs_get1_zsdt0347>-prazo_final.
                  else.
                    read table it_zsdt0347_aux assigning field-symbol(<fs_get2_zsdt0347>) with key uf = ''.
                    if sy-subrc = 0 .
                      _newval = <fs_get2_zsdt0347>-prazo_final.
                    endif.
                  endif.
                endif.
                "READ TABLE lt_zsdt0347 ASSIGNING FIELD-SYMBOL(<fs_zsdt0347>) INDEX _index.
*                WITH KEY safra   = <fs_zsdt0040>-safra
*                         cultura = <fs_zsdt0040>-cultura
*                         uf      = <fs_kna1>-regio
*                BINARY SEARCH.
                "IF sy-subrc IS NOT INITIAL .
                "153746 CS2023000827 Job Alt.Venc.Adiant.Cli. - P3 - PSA
**********************************************************************
*                  READ TABLE lt_zsdt0347 ASSIGNING <fs_zsdt0347>
*                  WITH KEY safra   = <fs_zsdt0040>-safra
*                  cultura = <fs_zsdt0040>-cultura
*                  uf      = space
*                  BINARY SEARCH.

*                IF sy-subrc = 0.
*                    CLEAR:_newval.
*                    _newval = <fs_zsdt0347>-prazo_final.
*                ELSE.
                "Se continuar sem prazo final pega da 41
                if _newval is initial.

                  read table lt_zsdt0041 assigning field-symbol(<fs_zsdt0041>)
                  with key vbeln = <fs_bsid>-vbel2
                  doc_simulacao = <fs_zsdt0040>-doc_simulacao
                  auart      = <fs_dados>-auart
                  spart = <fs_dados>-spart
                  binary search.

                  if sy-subrc = 0.
                    clear:_newval.
                    case <fs_zsdt0041>-spart.
                      when '02'.
                        _newval = <fs_zsdt0040>-dt_entrega_fet.
                      when '03'.
                        _newval = <fs_zsdt0040>-dt_entrega_def.
                      when '04'.
                        _newval = <fs_zsdt0040>-dt_entrega_sem.
                    endcase.
                  else.
                    read table lt_zsdt0090 assigning field-symbol(<fs_zsdt0090>)
                    with key vbeln = <fs_bsid>-vbel2
                    doc_simulacao = <fs_zsdt0040>-doc_simulacao
                    auart      = <fs_dados>-auart
                    spart = <fs_dados>-spart
                    binary search.

                    if sy-subrc = 0.
                      clear:_newval.
                      case <fs_zsdt0090>-spart.
                        when '02'.
                          _newval = <fs_zsdt0040>-dt_entrega_fet.
                        when '03'.
                          _newval = <fs_zsdt0040>-dt_entrega_def.
                        when '04'.
                          _newval = <fs_zsdt0040>-dt_entrega_sem.
                      endcase.
                    else.

                    endif.
                  endif.
                endif.
                "IF <fs_zsdt0347> IS ASSIGNED AND <fs_zsdt0347>-tipo_ov CS <fs_dados>-auart .
**********************************************************************
                if _newval is not initial.
                  refresh gt_accchg.
                  gs_accchg-fdname = 'ZFBDT'.
                  gs_accchg-newval =  _newval. "<fs_zsdt0347>-prazo_final.
                  append gs_accchg to gt_accchg.
                  clear gs_accchg.

                  call function 'FI_DOCUMENT_CHANGE'
                    exporting
                      i_awtyp              = <fs_bkpf>-awtyp
                      i_awref              = <fs_bkpf>-awkey+00(10)
                      i_aworg              = <fs_bkpf>-awkey+10(10)
                      i_buzei              = <fs_bsid>-buzei
                    tables
                      t_accchg             = gt_accchg
                    exceptions
                      no_reference         = 1
                      no_document          = 2
                      many_documents       = 3
                      wrong_input          = 4
                      overwrite_creditcard = 5
                      others               = 6.

                  update zsdt0041 set venc_editado = abap_true  where vbeln = <fs_dados>-vbeln.
                  if sy-subrc is initial.
                    commit work.
                  endif.

                  update zsdt0090 set venc_editado = abap_true where vbeln = <fs_dados>-vbeln.
                  if sy-subrc is initial.
                    commit work.
                  endif.
                endif.

                "ENDIF.

              endif.

            endif.

          endif.

        endif.

      endif.

      clear lv_achou.

    endloop.


  endif.
*** Fim - Rubenilson - 03.09.24 - US128351

*  LOOP AT TW_ZSDT0054.
*
*    READ TABLE TW_ZSDT0051 WITH KEY NRO_SOL_OV = TW_ZSDT0054-NRO_SOL_OV.
*    IF NOT SY-SUBRC IS INITIAL. CONTINUE. ENDIF.
*
*    READ TABLE TW_BSAD WITH KEY BUKRS = TW_ZSDT0051-VKORG
*                                KUNNR = TW_ZSDT0051-KUNNR
*                                BELNR = TW_ZSDT0054-ADIANT.
*    IF NOT SY-SUBRC IS INITIAL. CONTINUE. ENDIF.
*
*    READ TABLE TW_BSID WITH KEY BUKRS = TW_BSAD-BUKRS
*                                KUNNR = TW_BSAD-KUNNR
*                                BELNR = TW_BSAD-AUGBL.
*    IF NOT SY-SUBRC IS INITIAL. CONTINUE. ENDIF.
*
*    READ TABLE TW_BKPF WITH KEY BUKRS = TW_BSID-BUKRS
*                                BELNR = TW_BSID-BELNR
*                                GJAHR = TW_BSID-GJAHR.
*    IF NOT SY-SUBRC IS INITIAL. CONTINUE. ENDIF.
*
*    REFRESH GT_ACCCHG.
*    GS_ACCCHG-FDNAME = 'ZFBDT'.
*    GS_ACCCHG-NEWVAL =  TW_ZSDT0051-DTATE_LOGIST.
*    APPEND GS_ACCCHG TO GT_ACCCHG.
*    CLEAR GS_ACCCHG.
*
*    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
*      EXPORTING
*        I_AWTYP              = TW_BKPF-AWTYP
*        I_AWREF              = TW_BKPF-AWKEY+00(10)
*        I_AWORG              = TW_BKPF-AWKEY+10(10)
*        I_BUZEI              = TW_BSID-BUZEI
*      TABLES
*        T_ACCCHG             = GT_ACCCHG
*      EXCEPTIONS
*        NO_REFERENCE         = 1
*        NO_DOCUMENT          = 2
*        MANY_DOCUMENTS       = 3
*        WRONG_INPUT          = 4
*        OVERWRITE_CREDITCARD = 5
*        OTHERS               = 6.
*
*    IF SY-SUBRC IS INITIAL.
*
*      UPDATE ZSDT0054 SET ATUALIZADO = ABAP_TRUE
*            WHERE NRO_SOL_OV = TW_ZSDT0054-NRO_SOL_OV.
*
*      COMMIT WORK.
*
*    ENDIF.
*
*  ENDLOOP.

* [CS2016000946]<<<<

endform.
