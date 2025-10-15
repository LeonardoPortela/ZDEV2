class ZCL_CONT_CREDITO definition
  public
  final
  create public .

public section.

  types:
*---------------------------------------------------------------------*
* Declarações
*---------------------------------------------------------------------*
    BEGIN OF ty_auart,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE auart,
         high   TYPE auart,
       END OF ty_auart .
  types:
    BEGIN OF ty_werks,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE werks_d,
         high   TYPE werks_d,
       END OF ty_werks .
  types:
    BEGIN OF ty_matnr,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE matnr,
         high   TYPE matnr,
       END OF ty_matnr .
  types:
    BEGIN OF ty_vbeln,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE vbeln_vf,
         high   TYPE vbeln_vf,
       END OF ty_vbeln .
  types:
    BEGIN OF ty_vkorg,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE vkorg,
         high   TYPE vkorg,
       END OF ty_vkorg .
  types:
    BEGIN OF ty_matkl,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE matkl,
         high   TYPE matkl,
       END OF ty_matkl .

  data:
    lt_zib_contabil TYPE TABLE OF zib_contabil .
  data:
    lt_auart        TYPE TABLE OF ty_auart .
  data:
    lt_werks        TYPE TABLE OF ty_werks .
  data:
    lt_matnr        TYPE TABLE OF ty_matnr .
  data:
    lt_vbeln        TYPE TABLE OF ty_vbeln .
  data:
    lt_vkorg        TYPE TABLE OF ty_vkorg .
  data:
    lt_hkont        TYPE TABLE OF rgsb4 .
  data:
    lt_matkl        TYPE TABLE OF ty_matkl .
  data:
    lv_seq(6)       TYPE n .
  data LS_AUART type TY_AUART .
  data LS_WERKS type TY_WERKS .
  data LS_MATNR type TY_MATNR .
  data LS_VBELN type TY_VBELN .
  data LS_VKORG type TY_VKORG .
  constants LC_I type C value 'I' ##NO_TEXT.
  constants:
    lc_eq(2) TYPE c value 'EQ' ##NO_TEXT.
  data GT_WERKS like LT_WERKS .
  data GT_MATNR like LT_MATNR .
  data GT_AUART like LT_AUART .
  data GT_VBELN like LT_VBELN .
  data GT_MATKL like LT_MATKL .
  data AT_OBJ_KEY type AWKEY .

  methods PROCESS
    exporting
      value(LT_MESSAGE) type STRING .
  methods SET
    importing
      !LT_WERKS like LT_WERKS
      !LT_MATNR like LT_MATNR
      !LT_AUART like LT_AUART
      !LT_VBELN like LT_VBELN
      value(LT_MATKL) like LT_MATKL optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CONT_CREDITO IMPLEMENTATION.


  method process.

    types: begin of ty_material,
             matnr type mara-matnr,
             matkl type mara-matkl,
           end of ty_material.

    data: e_status(1),
          e_messa(64),
          it_material type table of ty_material,
          lv_cont     type char02 value '00'.
*---------------------------------------------------------------------*
* Busca informações relevantes para o processo
*---------------------------------------------------------------------*
* Buscar Impostos SD (MT) Selecionar na tabela ZSDT0245 o valor dos campos abaixo:
    select auart, kschl, werks, matnr, matkl, perc
      from zsdt0245 " Condições impostos MT (SD)
      into table @data(it_zsdt0245)
    where kschl in ('ZCP1', 'ZPP1')
      and auart in @gt_auart
      and werks in @gt_werks
      and matnr in @gt_matnr.

*      Buscar Impostos SD (MT) Selecionar na tabela ZSDT0245 o valor dos campos abaixo:
    select auart, kschl, werks, matnr, matkl, perc
      from zsdt0245 " Condições impostos MT (SD)
     appending table @it_zsdt0245
    where kschl in ('ZCP1', 'ZPP1')
      and auart in @gt_auart
      and werks in @gt_werks
      and matkl in @gt_matkl.
*      and matnr in @gt_matnr.

    free: it_material.
    if it_zsdt0245 is not initial.
      select matnr matkl from mara into table it_material
        for all entries in it_zsdt0245
        where matkl eq it_zsdt0245-matkl.
      if sy-subrc eq 0.
        loop at it_material assigning field-symbol(<ws_material>).
          read table it_zsdt0245 into data(ws_zsdt0245) with key matkl = <ws_material>-matkl.
          if sy-subrc eq 0.
            ws_zsdt0245-matnr = <ws_material>-matnr.
            append ws_zsdt0245 to it_zsdt0245.
          endif.
          clear: ws_zsdt0245.
        endloop.
      endif.
    endif.

    sort it_zsdt0245 by matnr.
*    delete adjacent duplicates from it_zsdt0245 comparing matnr.
    delete it_zsdt0245 where matnr eq space.


    if it_zsdt0245 is not initial.
      sort it_zsdt0245 by auart matnr werks.
      ls_auart = value #( sign = lc_i option = lc_eq low = space high = space ).
      ls_matnr = ls_auart.
      ls_werks = ls_auart.
      loop at it_zsdt0245 assigning field-symbol(<fs_zsdt0245>).
        ls_auart-low = <fs_zsdt0245>-auart.
        append ls_auart to lt_auart.
        ls_matnr-low = <fs_zsdt0245>-matnr.
        append ls_matnr to lt_matnr.
        ls_werks-low = <fs_zsdt0245>-werks.
        append ls_werks to lt_werks.
      endloop.

      sort lt_auart by low.
      delete adjacent duplicates from lt_auart comparing low.
      sort lt_matnr by low.
      delete adjacent duplicates from lt_matnr comparing low.
      sort lt_werks by low.
      delete adjacent duplicates from lt_werks comparing low.

      if sy-cprog eq 'ZFIS0001'.
        data lv_dat type sy-datum.
        lv_dat = sy-datum - 1.
* Buscar Faturamentos de Venda realizados conforme parâmetros
        select fkart, vbeln, vkorg, fkdat
          from vbrk
          into table @data(it_vbrk)
          where fkart in @lt_auart
            and vbeln in @gt_vbeln
            and fkdat le @sy-datum
            and fkdat ge @lv_dat.
      else.
* Buscar Faturamentos de Venda realizados conforme parâmetros
        select fkart, vbeln, vkorg, fkdat
          from vbrk
          into table @it_vbrk
          where fkart in @lt_auart
            and vbeln in @gt_vbeln
            and fkdat le @sy-datum.
      endif.
      if sy-subrc is initial.
        sort it_vbrk by fkart vbeln vkorg.
        sort it_vbrk by fkart.
        ls_vbeln = value #( sign = lc_i option = lc_eq low = space high = space ).
        ls_vkorg = ls_vbeln.
        loop at it_vbrk assigning field-symbol(<fs_vbrk>).
          ls_vbeln-low = <fs_vbrk>-vbeln.
          append ls_vbeln to lt_vbeln.
          ls_vkorg-low = <fs_vbrk>-vkorg.
          append ls_vkorg to lt_vkorg.
        endloop.

        sort lt_vbeln by low.
        delete adjacent duplicates from lt_vbeln comparing low.
        sort lt_vkorg by low.
        delete adjacent duplicates from lt_vkorg comparing low.

* Item Faturamento Selecionar na tabela VBRP conforme abaixo:
        select vbeln, matnr, werks, netwr
          from vbrp
          into table @data(it_vbrp)
          for all entries in @lt_vbeln
          where vbeln eq @lt_vbeln-low
            and matnr in @lt_matnr
            and werks in @lt_werks.
        if sy-subrc is initial.
          sort it_vbrp by vbeln matnr werks.
        endif.
      endif.

      call function 'G_SET_GET_ALL_VALUES'
        exporting
          class           = '0000'
          setnr           = 'MAGGI_CREDPRESUMIDO'
          no_descriptions = abap_off
          no_rw_info      = abap_off
        tables
          set_values      = lt_hkont
        exceptions
          set_not_found   = 1
          others          = 2.
      if sy-subrc is initial and lt_hkont is not initial.
        sort lt_hkont by title.
      else.
        if sy-batch is initial.
          message 'Set não foi criado ou definido (MAGGI_CREDPRESUMIDO)!'(i01) type 'I'.
          lt_message = text-i01.
        endif.
        exit.
      endif.

*---------------------------------------------------------------------*
* Processamento Contabilização e Cálculo PIS\COFINS Presumido
*---------------------------------------------------------------------*
      loop at it_vbrk assigning <fs_vbrk>.


        loop at it_vbrp assigning field-symbol(<fs_vbrp>) where vbeln = <fs_vbrk>-vbeln.

          select single vkorg from t001w where werks = @<fs_vbrp>-werks into @data(_empresa).

          "Verifica periodo aberto.
          if sy-subrc = 0.
            call function 'Z_CONTROLE_FECHAMES'
              exporting
                i_bukrs  = _empresa
                i_data   = <fs_vbrk>-fkdat
              importing
                e_status = e_status
                e_messa  = e_messa
              exceptions
                error    = 1
                others   = 2.

            if  e_status = 'E'.
              message e000(z01) with e_messa.
              continue.
            endif.
          endif.

          loop at it_zsdt0245 assigning <fs_zsdt0245> where  auart = <fs_vbrk>-fkart
                                                        and  matnr = <fs_vbrp>-matnr
                                                        and  werks = <fs_vbrp>-werks.

            free lv_seq.
            if <fs_zsdt0245>-kschl eq 'ZCP1'.
              lv_seq = 2.
            endif.
            do 2 times.

              append initial line to lt_zib_contabil assigning field-symbol(<fs_zib_contabil>).

              lv_seq = lv_seq + 1.
              <fs_zib_contabil>-obj_key  = |ZFIS{ <fs_vbrk>-vbeln }{ lv_cont }{ <fs_vbrk>-fkdat(4) }|.


              select * from zfit0229 into table @data(it_zfit0229) where obj_key_ref = @<fs_zib_contabil>-obj_key order by obj_key descending.
              if sy-subrc eq 0.
              read table it_zfit0229 into data(ws_zfit0229) with key obj_key_ref = <fs_zib_contabil>-obj_key.
                "===============================================================Alteração nova chave do objeto devido estorno.
                lv_cont = |{ <fs_zib_contabil>-obj_key+14(2) }|.
                lv_cont = lv_cont + 01.
                lv_cont = |{ lv_cont alpha = in }|.
                <fs_zib_contabil>-obj_key = |ZFIS{ <fs_vbrk>-vbeln }{ lv_cont }{ <fs_vbrk>-fkdat(4) }|.
                lv_cont = '00'.
              endif.
              "===============================================================Alteração nova chave do objeto devido estorno.

              <fs_zib_contabil>-seqitem         = lv_seq.
              if lv_seq eq 1 or lv_seq eq 3.
                <fs_zib_contabil>-bschl           = '40'.
              else.
                <fs_zib_contabil>-bschl           = '50'.
              endif.
              <fs_zib_contabil>-gsber           = <fs_vbrp>-werks.
              <fs_zib_contabil>-bukrs           = <fs_vbrk>-vkorg.
              <fs_zib_contabil>-interface       = '0'.
              <fs_zib_contabil>-bktxt           = 'Credito Presumido'(t02).
              <fs_zib_contabil>-bldat           = <fs_vbrk>-fkdat+6(2) && '.' && <fs_vbrk>-fkdat+4(2) && '.' && <fs_vbrk>-fkdat(4).
              <fs_zib_contabil>-budat           = <fs_vbrk>-fkdat+6(2) && '.' && <fs_vbrk>-fkdat+4(2) && '.' && <fs_vbrk>-fkdat(4).
              <fs_zib_contabil>-gjahr           = <fs_vbrk>-fkdat(4).
              <fs_zib_contabil>-monat           = <fs_vbrk>-fkdat+4(2).
              <fs_zib_contabil>-blart           = 'LM'(t03).
              <fs_zib_contabil>-xblnr           = <fs_vbrk>-vbeln.
              read table lt_hkont assigning field-symbol(<fs_hkont>) with key title(4)   = <fs_zsdt0245>-kschl
                                                                              title+8(2) = <fs_zib_contabil>-bschl
                                                                     binary search.
              if sy-subrc is initial.
                <fs_zib_contabil>-hkont           = <fs_hkont>-from.
              else.
                free <fs_zib_contabil>-hkont.
              endif.

              if <fs_zsdt0245>-perc is initial or <fs_vbrp>-netwr is initial.
                <fs_zib_contabil>-interface       = '9'.
                continue.
              endif.

              <fs_zib_contabil>-wrbtr           = ( ( <fs_vbrp>-netwr * ( <fs_zsdt0245>-perc ) ) / 100 ).
              <fs_zib_contabil>-waers           = 'BRL'(t04).
              <fs_zib_contabil>-bupla           = <fs_vbrp>-werks.
              <fs_zib_contabil>-zuonr           = 'Cred.Pres.Vd.Fab'(t01).
              <fs_zib_contabil>-rg_atualizado   = 'N'.

            enddo.
          endloop.
          if sy-subrc is not initial.
            data(lv_errot) = abap_true.
          endif.
        endloop.
      endloop.

      if sy-batch is initial and lv_errot is not initial and it_vbrk[] is not initial.
        message 'Material(is) não encontrado(s) na ZSDT0245!'(e02) type 'E'.
        lt_message = text-e02.
      endif.

      if lt_zib_contabil[] is not initial.
        delete lt_zib_contabil where interface eq '9'.
        if lt_zib_contabil[] is not initial.
          modify zib_contabil from table lt_zib_contabil.
          if sy-subrc is initial.
            commit work and wait.
            if sy-batch is initial.
*              MESSAGE 'Processamento realizado com sucesso!'(s03) TYPE 'E'.
              lt_message = text-s03.
*                  DATA(lv_ok) = abap_true.
*                  EXPORT lv_ok FROM lv_ok TO MEMORY ID 'LV_OK_ZFIS0001'.
            endif.
          endif.
        else.
          if sy-batch is initial and it_vbrk[] is not initial.
*            MESSAGE 'Nenhum documento gerado na lt_zib_contabil!'(e03) TYPE 'E'.
            lt_message = text-e03.
          endif.
        endif.
      else.
        if sy-batch is initial.
*          MESSAGE 'Nenhum documento gerado na lt_zib_contabil!'(e03) TYPE 'E'.
          lt_message = text-e03.
        endif.
      endif.
    else.
*      IF sy-batch IS INITIAL AND lv_errot IS NOT INITIAL AND it_vbrk[] IS NOT INITIAL.
*      MESSAGE 'Material(is) não encontrado(s) na ZSDT0245!'(e02) TYPE 'E'.
      lt_message = text-e02.
*      ENDIF.
    endif.
  endmethod.


  method set.
    gt_werks[] = lt_werks[].
    gt_matnr[] = lt_matnr[].
    gt_auart[] = lt_auart[].
    gt_vbeln[] = lt_vbeln[].
    gt_matkl[] = lt_MATKL[].
  endmethod.
ENDCLASS.
