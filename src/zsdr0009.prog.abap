*&---------------------------------------------------------------------*
*& Report  ZSDR0009
*&
*&---------------------------------------------------------------------*
*&  carga das informações de exportação na tabela ZLFT_EXPORTACAO,
*&  esse programa deve ser executado antes da interface de RFC
*&---------------------------------------------------------------------*
*&**********************Histórico de Alterações ************************
*& Data           Autor           Request         Descrição            *
*& 03.06.2014     Marcos Faneli                   CH. 126388           *
*&---------------------------------------------------------------------*

report  zsdr0009.

type-pools: icon.

*TYPES: BEGIN OF ty_cfop.
*TYPES:  sign   TYPE  char1,
*        option TYPE  char2,
*        low     TYPE char10,
*        high   TYPE char10.
*TYPES: END OF ty_cfop.

*TYPES: BEGIN OF ty_message.
*        INCLUDE STRUCTURE zlft_saida.
*TYPES:  tipo              TYPE c LENGTH 1,
*        menssagem         TYPE char100,
*        ds_nome_transpor  TYPE char50.
*TYPES: END OF ty_message.

*DATA: it_zlft_saida       TYPE TABLE OF zlft_saida INITIAL SIZE 0 WITH HEADER LINE,
*      it_cfop             TYPE TABLE OF ty_cfop,
*      it_zdoc_memorando   TYPE TABLE OF zdoc_memorando INITIAL SIZE 0 WITH HEADER LINE,
*      it_zdoc_rem_bl      TYPE TABLE OF zdoc_rem_bl INITIAL SIZE 0 WITH HEADER LINE,
*      it_messagem         TYPE TABLE OF ty_message INITIAL SIZE 0 WITH HEADER LINE,
*      wa_messagem         TYPE ty_message,
*      wa_cfop             TYPE ty_cfop,
*      vg_j_1bnfnumb       TYPE j_1bnfnumb,
*      vg_refkey           TYPE vbeln,
*      wa_zlft_saida       TYPE zlft_saida,
*      vg_land1            TYPE land1.
tables : zdde.

types: begin of ty_zlft_exportacao.
        include type zlft_exportacao.
types: bacen             type c length 120,
       pais_dest        type n length 3,
       num_conhec       type c length 120,
       tp_conhect       type c length 120,
       data_emb         type c length 255,
*       pais_dest_export type c length 4,
end of ty_zlft_exportacao.

types: begin of ty_znom_conhec,
        id_conhec        type znom_conhec-id_conhec,
        id_nomeacao_tran type znom_conhec-id_nomeacao_tran,
        sg_pais_destino  type znom_conhec-sg_pais_destino,
        nr_conhec        type znom_conhec-nr_conhec,
        ds_tipo          type znom_conhec-ds_tipo,
        dt_data          type znom_conhec-dt_data,
        land1            type zpais-land1,
      end of ty_znom_conhec,

      begin of ty_vbrp ,
        vbeln  type j_1bnflin-refkey,
        vgbel  type vbrp-vgbel,
        netwr  type vbrp-netwr,
      end   of ty_vbrp,

      begin of ty_zreg_exportacao,
        id_registro_expo  type zreg_exportacao-id_registro_expo,
        id_nomeacao_tran  type znom_transporte-id_nomeacao_tran,
        nr_valor_total    type zreg_exportacao-nr_valor_total  ,
        dt_registro_expo  type zreg_exportacao-dt_registro_expo,
        id_pais_destino   type zreg_exportacao-id_pais_destino,
      end   of ty_zreg_exportacao.

data: t_j_1bnfdoc        type table of j_1bnfdoc,
      t_j_1bnflin        type table of j_1bnflin,
      t_zdoc_exp         type table of zdoc_exp,
      t_vbrp             type table of ty_vbrp,
      t_vbrp_aux         type table of vbrp,
      t_zreg_exportacao  type table of ty_zreg_exportacao,
      t_zreg_export_aux  type table of zreg_exportacao,
      t_zdoc_rem_bl      type table of zdoc_rem_bl,
      t_znom_transporte  type table of znom_transporte,
      t_zdde_aplicacao   type table of zdde_aplicacao,
      t_zdde             type table of zdde,
      t_zsdt0170         type table of zsdt0170,
      T_ZSDT0170_REF     TYPE TABLE OF ZSDT0170,
      t_zsdt0170_retific type table of zsdt0170,
      t_zsdt0172         type table of zsdt0172,
      t_zsdt0174         type table of zsdt0174,
      t_znom_conhec      type table of ty_znom_conhec,
      t_zpais            type table of zpais,
      t_zdoc_memorando   type table of zdoc_memorando,
      t_zlft_exportacao  type table of ty_zlft_exportacao.

data: wa_j_1bnfdoc        type j_1bnfdoc,
      wa_j_1bnflin        type j_1bnflin,
      wa_zdoc_exp         type zdoc_exp,
      wa_vbrp             type ty_vbrp,
      wa_vbrp_aux         type vbrp,
      wa_zreg_exportacao  type ty_zreg_exportacao,
      wa_zreg_export_aux  type zreg_exportacao,
      wa_zdoc_rem_bl      type zdoc_rem_bl,
      wa_znom_transporte  type znom_transporte,
      wa_zdde_aplicacao   type zdde_aplicacao,
      wa_zdde             type zdde,
      wa_zsdt0170         type zsdt0170,
      wa_zsdt0172         type zsdt0172,
      wa_zsdt0174         type zsdt0174,
      wa_znom_conhec      type ty_znom_conhec,
      wa_znom_conhec_aux  type znom_conhec,
      wa_zpais            type zpais,
      wa_zdoc_memorando   type zdoc_memorando,
      wa_zlft_exportacao  type ty_zlft_exportacao.

data: vg_txjcd           like t001w-txjcd.

*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*
data:
      wa_cont             type ref to cl_gui_custom_container,
      wa_alv              type ref to cl_gui_alv_grid,
      wa_layout           type lvc_s_layo   .

data: it_fcat             type table of lvc_s_fcat,
      gs_alv_refres_cond  type lvc_s_stbl,
      formapgto           type zib_contabil-zlsch,
      bcoempresa          type zib_contabil-hbkid,
      dtvencto            type zib_contabil-zfbdt,
      proc_v              type c length 1,
      gs_variant_c        type disvariant.


*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.
select-options: p_dt_av for zdde-dt_averbacao.
selection-screen: end of block b1.

start-of-selection.

  delete from zlft_exportacao.
  commit work.

  perform : zseleciona_dados,
            zprocessa_dados_dde,
            zprocessa_dados_due,

            zalv. " Form ALV.

  call screen 0100.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  zseleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zseleciona_dados.
  data: tl_re type range of znr_reg_ex with header line.

  field-symbols: <fs_znom_conhec> type ty_znom_conhec,
                 <fs_zdoc_exp>    type zdoc_exp.

*---------------------------------------------------------*
* Busca Dados DU-e
*---------------------------------------------------------*
  select *
    from zsdt0170 as a into table t_zsdt0170
   where situacao_due  EQ '70' "Averbada
     and dt_situacao   in p_dt_av
     and performance   eq abap_false
     AND exists ( select *
                    from zdoc_exp as b
                   where b~id_due = a~id_due ).

  IF t_zsdt0170[] is NOT INITIAL.

    "Paises DU-e
    select *
      from zsdt0174 into table t_zsdt0174
       FOR ALL ENTRIES IN t_zsdt0170
     WHERE ID_DUE EQ t_zsdt0170-ID_DUE.

    SORT t_zsdt0174 by id_due destino_country.
    delete adjacent duplicates from t_zsdt0174 comparing id_due.

    LOOP AT T_ZSDT0170 INTO WA_ZSDT0170 WHERE ID_DUE_REF IS NOT INITIAL. "Caso tenha DU-e's com referencia

      SELECT *
        FROM ZSDT0170 INTO TABLE T_ZSDT0170_REF
         FOR ALL ENTRIES IN T_ZSDT0170
       WHERE ID_DUE = T_ZSDT0170-ID_DUE_REF.

      EXIT.
    ENDLOOP.

    LOOP AT T_ZSDT0170_REF INTO DATA(_WL_0170_REF).
      READ TABLE T_ZSDT0170 ASSIGNING FIELD-SYMBOL(<FS_ZSDT0170>) WITH KEY ID_DUE_REF = _WL_0170_REF-ID_DUE.
      CHECK ( SY-SUBRC EQ 0 ).

      IF <FS_ZSDT0170>-DT_REGISTRO_PORTAL IS INITIAL.
        <FS_ZSDT0170>-DT_REGISTRO_PORTAL = _WL_0170_REF-DT_REGISTRO_PORTAL.
      ENDIF.
    ENDLOOP.

*    select *
*      from zsdt0170 into table t_zsdt0170_retific
*       FOR ALL ENTRIES IN t_zsdt0170
*     WHERE ID_DUE_REF EQ t_zsdt0170-ID_DUE
*       AND TP_DUE     EQ '2'.
*
*    IF t_zsdt0170_retific[] is NOT INITIAL.
*
*      select *
*        from zsdt0174 appending table t_zsdt0174
*         FOR ALL ENTRIES IN t_zsdt0170_retific
*       WHERE ID_DUE EQ t_zsdt0170_retific-ID_DUE.
*
*    ENDIF.

  ENDIF.

*---------------------------------------------------------*
* Busca Dados DDE/RE
*---------------------------------------------------------*
  select *
    into table t_zdde
    from zdde
   where dt_averbacao in p_dt_av.

  if t_zdde[] IS NOT INITIAL.
    select *
      into table t_zdde_aplicacao
      from zdde_aplicacao
       for all entries in t_zdde
     where id_dde eq t_zdde-id_dde.

    if t_zdde_aplicacao[] is NOT initial.
      select *
        into table t_zreg_export_aux
        from zreg_exportacao
         for all entries in t_zdde_aplicacao
       where id_registro_expo = t_zdde_aplicacao-id_registro_expo
         and in_performance   = ''. "17.04.2017 CS2017000801
    ENDIF.
  ENDIF.

  loop at t_zreg_export_aux into wa_zreg_export_aux.
    move-corresponding wa_zreg_export_aux to wa_zreg_exportacao.
    wa_zreg_exportacao-id_nomeacao_tran = wa_zreg_export_aux-id_nomeacao_tran.

    append wa_zreg_exportacao to t_zreg_exportacao.
  endloop.

  "Carregar Nomeação Transporte
  if t_zreg_exportacao[] is not initial.
    select *
      into table t_znom_transporte
      from znom_transporte
       for all entries in t_zreg_exportacao
     where id_nomeacao_tran eq t_zreg_exportacao-id_nomeacao_tran.
  endif.

  if t_zsdt0170[] is not initial.
    select *
      APPENDING table t_znom_transporte
      from znom_transporte
       for all entries in t_zsdt0170
     where id_nomeacao_tran eq t_zsdt0170-id_nomeacao_tran.
  endif.

  sort t_znom_transporte by id_nomeacao_tran.
  delete adjacent duplicates from t_znom_transporte comparing id_nomeacao_tran.

  "Carrega Documentos Exportação
  IF t_zreg_exportacao[] IS NOT INITIAL.
    select distinct *
      into table t_zdoc_exp
      from zdoc_exp as a
       for all entries in t_zreg_exportacao
     where id_registro_expo = t_zreg_exportacao-id_registro_expo
       and not exists ( select *
                          from zdoc_exp_recusa as b
                         WHERE b~id_doc_exp = a~id_doc_exp ).
  ENDIF.

  IF t_zsdt0170[] IS NOT INITIAL.
    select distinct *
      from zdoc_exp as a APPENDING table t_zdoc_exp
       for all entries in t_zsdt0170
     where id_due = t_zsdt0170-id_due
       and not exists ( select *
                          from zdoc_exp_recusa as b
                         WHERE b~id_doc_exp = a~id_doc_exp ).
  ENDIF.

  if t_zdoc_exp[] is not initial.
    free: tl_re.

    loop at t_zdoc_exp assigning <fs_zdoc_exp>.

      CHECK <fs_zdoc_exp>-nr_registro_expo is NOT INITIAL.

      clear tl_re.
      tl_re-low    = <fs_zdoc_exp>-nr_registro_expo.
      replace all occurrences of regex '[^0-9]' in tl_re-low with '' ignoring case.
      tl_re-sign   = 'I'.
      tl_re-option = 'EQ'.
      append tl_re.

    endloop.

    unassign <fs_zdoc_exp>.

** Encontrar números de memorando
    if tl_re[] is NOT INITIAL.
      select distinct *
        into table t_zdoc_memorando
        from zdoc_memorando
       where nr_re in tl_re.
    endif.

    select *
      into table t_zdoc_rem_bl
      from zdoc_rem_bl
       for all entries in t_zdoc_exp
     where id_doc_exp eq t_zdoc_exp-id_doc_exp.

    select id_conhec id_nomeacao_tran sg_pais_destino nr_conhec ds_tipo dt_data
      into table t_znom_conhec
      from znom_conhec
       for all entries in t_zdoc_exp
     where id_nomeacao_tran eq t_zdoc_exp-id_nomeacao_tran.

** Busca código do BACEM para os países
*    IF T_ZNOM_CONHEC IS NOT INITIAL.
*      LOOP AT T_ZNOM_CONHEC ASSIGNING <FS_ZNOM_CONHEC>.
*        <FS_ZNOM_CONHEC>-LAND1 = <FS_ZNOM_CONHEC>-SG_PAIS_DESTINO.
*      ENDLOOP.
*
*      UNASSIGN <FS_ZNOM_CONHEC>.
*
*      SELECT *
*        INTO TABLE T_ZPAIS
*        FROM ZPAIS
*         FOR ALL ENTRIES IN T_ZNOM_CONHEC
*       WHERE LAND1 EQ T_ZNOM_CONHEC-LAND1.
*
*    ENDIF.

    "Carregar Pais
    if t_zreg_exportacao[] is NOT INITIAL.

      select *
        into table t_zpais
        from zpais
         for all entries in t_zreg_exportacao
       where land1 eq t_zreg_exportacao-id_pais_destino.

    endif.

    if t_zsdt0174[] is NOT INITIAL.

      select *
        appending table t_zpais
        from zpais
         for all entries in t_zsdt0174
       where land1 eq t_zsdt0174-destino_country.

    endif.

    select *
      into table t_vbrp_aux
      from vbrp
       for all entries in t_zdoc_exp
     where vgbel eq t_zdoc_exp-vbeln AND DRAFT = SPACE .

    loop at t_vbrp_aux into wa_vbrp_aux.
      move-corresponding wa_vbrp_aux to wa_vbrp.
      wa_vbrp-vbeln = wa_vbrp_aux-vbeln.

      append wa_vbrp to t_vbrp.

    endloop.

    if sy-subrc is initial.
      select *
        into table t_j_1bnflin
        from j_1bnflin
         for all entries in t_vbrp
       where refkey = t_vbrp-vbeln.

      select *
        into table t_j_1bnfdoc
        from j_1bnfdoc
         for all entries in t_j_1bnflin
       where docnum = t_j_1bnflin-docnum.

    endif.
  endif.



endform.                    "zseleciona_dados

*&---------------------------------------------------------------------*
*&      Form  zprocessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zprocessa_dados_dde.
  data: vl_data_emb type c length 10.

  sort: t_zdde_aplicacao  by id_dde,
        t_zreg_exportacao by id_registro_expo,
        t_znom_transporte by id_nomeacao_tran,
        t_zdoc_exp        by id_registro_expo,
        t_zdoc_rem_bl     by id_doc_exp,
        t_znom_conhec     by id_nomeacao_tran ,
        t_vbrp            by vgbel,
        t_j_1bnflin       by refkey,
        t_j_1bnfdoc       by docnum.

  loop at t_zdde into wa_zdde.
    write: wa_zdde-dt_dde       to wa_zlft_exportacao-dt_declaracao,
           wa_zdde-dt_averbacao to wa_zlft_exportacao-dt_averbacao_dec,
           wa_zdde-nr_dde       to wa_zlft_exportacao-num_desp_export.

    move: wa_zdde-nr_valor     to wa_zlft_exportacao-vlr_desp_export.

    loop at t_zdde_aplicacao into wa_zdde_aplicacao where id_dde = wa_zdde-id_dde .
      clear : wa_zreg_exportacao,
              wa_znom_transporte,
              wa_zdoc_exp,
              wa_zdoc_rem_bl,
              wa_znom_conhec,
              wa_zpais,wa_vbrp,
              wa_j_1bnflin,
              wa_j_1bnfdoc.

      read table t_zreg_exportacao into wa_zreg_exportacao with key id_registro_expo = wa_zdde_aplicacao-id_registro_expo binary search.
      if sy-subrc is initial.
        wa_zreg_exportacao-id_pais_destino = wa_zreg_exportacao-id_pais_destino.
      endif.
      write: wa_zreg_exportacao-dt_registro_expo to wa_zlft_exportacao-dt_registro.

*      READ TABLE T_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE WITH KEY ID_NOMEACAO_TRAN = WA_ZREG_EXPORTACAO-ID_NOMEACAO_TRAN BINARY SEARCH.

      loop at t_zdoc_exp into wa_zdoc_exp where id_registro_expo = wa_zreg_exportacao-id_registro_expo.
        wa_zlft_exportacao-num_re_export    = wa_zdoc_exp-nr_registro_expo.

        replace all occurrences of regex '[^0-9]' in wa_zdoc_exp-nr_registro_expo with '' ignoring case.
        read table t_zdoc_memorando into wa_zdoc_memorando with key nr_re = wa_zdoc_exp-nr_registro_expo.
        if sy-subrc is initial.
          wa_zlft_exportacao-num_memo_export = wa_zdoc_memorando-numero_memo.
        endif.

*        READ TABLE T_ZDOC_REM_BL INTO WA_ZDOC_REM_BL WITH KEY ID_DOC_EXP = WA_ZDOC_EXP-ID_DOC_EXP BINARY SEARCH.

        read table t_vbrp into wa_vbrp with key vgbel = wa_zdoc_exp-vbeln binary search.
        move : wa_vbrp-netwr     to wa_zlft_exportacao-vlr_re_export.

        read table t_j_1bnflin into wa_j_1bnflin with key refkey = wa_vbrp-vbeln binary search.
        wa_zlft_exportacao-cod_cfop_legal   = wa_j_1bnflin-cfop(4).

        read table t_j_1bnfdoc into wa_j_1bnfdoc with key docnum = wa_j_1bnflin-docnum binary search.
        call function 'J_1B_BRANCH_DETERMINE'
          exporting
            plant                    = wa_j_1bnfdoc-branch
          importing
            txjcd                    = vg_txjcd
          exceptions
            branch_not_found         = 1
            plant_not_found          = 2
            valuation_area_not_found = 3
            company_not_found        = 4
            others                   = 5.

        wa_zlft_exportacao-cod_municipio    = vg_txjcd+3(10).

        concatenate wa_j_1bnfdoc-docdat+6(2) '.' wa_j_1bnfdoc-docdat+4(2) '.' wa_j_1bnfdoc-docdat(4) into wa_zlft_exportacao-dt_emissao.
        wa_zlft_exportacao-cod_holding      = 'MAGGI'.
        wa_zlft_exportacao-cod_matriz       = wa_j_1bnfdoc-bukrs.
        wa_zlft_exportacao-cod_filial       = wa_j_1bnfdoc-branch.
        wa_zlft_exportacao-serie            = wa_j_1bnfdoc-series.
        wa_zlft_exportacao-subserie         = wa_j_1bnfdoc-subser.
        if wa_j_1bnfdoc-nfe eq 'X'.
          wa_zlft_exportacao-num_nf = wa_j_1bnfdoc-nfenum.
        else.
          wa_zlft_exportacao-num_nf = wa_j_1bnfdoc-nfnum.
        endif.

        clear: wa_zlft_exportacao-pais_dest,
               wa_zlft_exportacao-num_conhec,
               wa_zlft_exportacao-tp_conhect,
               wa_zlft_exportacao-data_emb.

        loop at t_znom_conhec into wa_znom_conhec where id_nomeacao_tran eq wa_zdoc_exp-id_nomeacao_tran.
          clear wa_zpais.
*          READ TABLE T_ZPAIS INTO WA_ZPAIS WITH KEY LAND1 = WA_ZNOM_CONHEC-LAND1.
*          IF SY-SUBRC IS INITIAL.
**            IF WA_ZLFT_EXPORTACAO-PAIS_DEST IS NOT INITIAL.
**              CONCATENATE WA_ZLFT_EXPORTACAO-BACEN WA_ZPAIS-BACEN(3) INTO WA_ZLFT_EXPORTACAO-BACEN SEPARATED BY ','.
**              CONCATENATE WA_ZLFT_EXPORTACAO-PAIS_DEST WA_ZNOM_CONHEC-SG_PAIS_DESTINO INTO WA_ZLFT_EXPORTACAO-PAIS_DEST SEPARATED BY ','.
**            ELSE.
**              WA_ZLFT_EXPORTACAO-BACEN = WA_ZPAIS-BACEN(3).
**              WA_ZLFT_EXPORTACAO-PAIS_DEST = WA_ZNOM_CONHEC-SG_PAIS_DESTINO.
*
*
*            WA_ZLFT_EXPORTACAO-PAIS_DEST = WA_ZPAIS-BACEN(3).
**            ENDIF.
*          ENDIF.

          read table t_zpais into wa_zpais with key land1 = wa_zreg_exportacao-id_pais_destino.
          if sy-subrc is initial.
            wa_zlft_exportacao-pais_dest = wa_zpais-bacen(3).
            wa_zlft_exportacao-pais_dest_export = wa_zlft_exportacao-pais_dest.
          endif.

          if wa_zlft_exportacao-num_conhec is not initial.
            concatenate wa_zlft_exportacao-num_conhec wa_znom_conhec-nr_conhec into wa_zlft_exportacao-num_conhec separated by ','.
          else.
            wa_zlft_exportacao-num_conhec = wa_znom_conhec-nr_conhec.
          endif.

          replace all occurrences of '/' in wa_znom_conhec-ds_tipo with ''.
          condense wa_znom_conhec-ds_tipo no-gaps.
*          IF WA_ZLFT_EXPORTACAO-TP_CONHECT IS NOT INITIAL.
          if sy-subrc is not initial.
*            CONCATENATE WA_ZLFT_EXPORTACAO-TP_CONHECT WA_ZNOM_CONHEC-DS_TIPO INTO WA_ZLFT_EXPORTACAO-TP_CONHECT SEPARATED BY ','.
*          ELSE.
            wa_zlft_exportacao-tp_conhect = wa_znom_conhec-ds_tipo.
          endif.

          concatenate wa_znom_conhec-dt_data+6(2) wa_znom_conhec-dt_data+4(2) wa_znom_conhec-dt_data(4) into vl_data_emb separated by '.'.
          if wa_zlft_exportacao-data_emb is not initial.
            concatenate wa_zlft_exportacao-data_emb vl_data_emb into wa_zlft_exportacao-data_emb   separated by ','.
          else.
            wa_zlft_exportacao-data_emb = vl_data_emb.
          endif.
        endloop.
        """""""""
        read table t_znom_conhec into wa_znom_conhec with key id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.
        read table t_zreg_exportacao into wa_zreg_exportacao with key id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.
        if sy-subrc is initial.
          read table t_zpais into wa_zpais with key land1 = wa_zreg_exportacao-id_pais_destino.
          if sy-subrc is initial.
*            WA_ZLFT_EXPORTACAO-PAIS_DEST_EXPORT = WA_ZPAIS-BACEN(3).
            wa_zlft_exportacao-pais_dest = wa_zpais-bacen(3).
            wa_zlft_exportacao-pais_dest_export = wa_zlft_exportacao-pais_dest.
          endif.

          replace all occurrences of '/' in wa_znom_conhec-ds_tipo with ''.
          wa_zlft_exportacao-tp_conhecimento  = wa_znom_conhec-ds_tipo.
          wa_zlft_exportacao-num_conhec_emb   = wa_znom_conhec-nr_conhec.
        endif.

        wa_zlft_exportacao-cod_moeda_export = 'USD'.
        wa_zlft_exportacao-usuario          = sy-uname.
        wa_zlft_exportacao-horatual         = sy-uzeit.

        write: sy-datlo to wa_zlft_exportacao-datatual.

        modify zlft_exportacao from wa_zlft_exportacao.

        append wa_zlft_exportacao to t_zlft_exportacao.

      endloop.
    endloop.

  endloop.

  commit work.

endform.                    "zprocessa


*  wa_cfop-sign   = 'I'.
*  wa_cfop-option = 'CP'.
*  wa_cfop-low    = '7*'.
*  APPEND wa_cfop TO it_cfop.
*
*  DELETE FROM zlft_exportacao.
*  COMMIT WORK.
*
*  SELECT *
*    INTO TABLE it_zlft_saida
*    FROM zlft_saida
*   WHERE cod_cfop_legal IN it_cfop.
*
*  LOOP AT it_zlft_saida INTO wa_zlft_saida.
*
*    CLEAR: wa_j_1bnfdoc, wa_j_1bnflin, vg_j_1bnfnumb, wa_zlft_exportacao, wa_messagem.
*
*    MOVE-CORRESPONDING wa_zlft_saida TO wa_zlft_exportacao.
*    MOVE-CORRESPONDING wa_zlft_saida TO wa_messagem.
*
*    IF wa_zlft_saida-cod_modelo EQ '55'.
*      SELECT SINGLE * INTO wa_j_1bnfdoc
*        FROM j_1bnfdoc
*       WHERE direct EQ '2'
*         AND bukrs  EQ wa_zlft_exportacao-cod_matriz
*         AND branch EQ wa_zlft_exportacao-cod_filial
*         AND series EQ wa_zlft_exportacao-serie
*         AND model  EQ '55'
*         AND nfe    EQ 'X'
*         AND nfenum EQ wa_zlft_exportacao-num_nf.
*    ELSE.
*      WRITE wa_zlft_exportacao-num_nf TO vg_j_1bnfnumb.
*      SELECT SINGLE * INTO wa_j_1bnfdoc
*        FROM j_1bnfdoc
*       WHERE direct EQ '2'
*         AND bukrs  EQ wa_zlft_exportacao-cod_matriz
*         AND branch EQ wa_zlft_exportacao-cod_filial
*         AND series EQ wa_zlft_exportacao-serie
*         AND nfnum  EQ vg_j_1bnfnumb.
*    ENDIF.
*
*    IF wa_j_1bnfdoc IS INITIAL.
*      wa_messagem-tipo      = 'E'.
*      wa_messagem-menssagem = 'Nota fiscal não localizada!'.
*      APPEND wa_messagem TO it_messagem.
*      CONTINUE.
*    ENDIF.
*
*    CALL FUNCTION 'J_1B_BRANCH_DETERMINE'
*      EXPORTING
*        plant                    = wa_j_1bnfdoc-branch
*      IMPORTING
*        txjcd                    = vg_txjcd
*      EXCEPTIONS
*        branch_not_found         = 1
*        plant_not_found          = 2
*        valuation_area_not_found = 3
*        company_not_found        = 4
*        OTHERS                   = 5.
*    IF sy-subrc <> 0.
*      wa_messagem-tipo      = 'W'.
*      wa_messagem-menssagem = 'Filial da Nota fiscal não localizada!'.
*      APPEND wa_messagem TO it_messagem.
*    ELSE.
*      wa_zlft_exportacao-cod_municipio = vg_txjcd+3(10).
*    ENDIF.
*
*    SELECT SINGLE * INTO wa_j_1bnflin
*      FROM j_1bnflin
*     WHERE docnum EQ wa_j_1bnfdoc-docnum.
*
*    IF NOT sy-subrc IS INITIAL.
*      wa_messagem-tipo      = 'W'.
*      wa_messagem-menssagem = 'Item da Nota fiscal não localizado!'.
*      APPEND wa_messagem TO it_messagem.
*    ENDIF.
*
*    IF wa_j_1bnflin-reftyp EQ 'BI'.
*
*      vg_refkey = wa_j_1bnflin-refkey(10).
*
*      IF NOT vg_refkey IS INITIAL.
*
*        SELECT SINGLE * INTO wa_vbrp
*          FROM vbrp
*         WHERE vbeln EQ vg_refkey.
*
*        IF sy-subrc IS INITIAL.
*
*          SELECT SINGLE * INTO wa_zdoc_exp
*            FROM zdoc_exp
*           WHERE vbeln EQ wa_vbrp-vgbel.
*
*          IF sy-subrc IS INITIAL.
*
*            wa_zlft_exportacao-num_re_export = wa_zdoc_exp-nr_registro_expo.
*            SELECT SINGLE * INTO wa_zreg_exportacao
*              FROM zreg_exportacao
*             WHERE id_registro_expo EQ wa_zdoc_exp-id_registro_expo.
*
*            "Achou RE
*            IF sy-subrc IS INITIAL.
*              MOVE: wa_zreg_exportacao-nr_valor_total   TO wa_zlft_exportacao-vlr_re_export.
*              WRITE: wa_zreg_exportacao-dt_registro_expo TO wa_zlft_exportacao-dt_registro.
*
*              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*              "" BUSCA NOMEACAO """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*              IF NOT wa_zreg_exportacao-id_nomeacao_tran IS INITIAL.
*                SELECT SINGLE * INTO wa_znom_transporte
*                  FROM znom_transporte
*                 WHERE id_nomeacao_tran EQ wa_zreg_exportacao-id_nomeacao_tran.
*                IF sy-subrc IS INITIAL.
*                  wa_messagem-ds_nome_transpor = wa_znom_transporte-ds_nome_transpor.
*                ENDIF.
*              ENDIF.
*
*              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*              "" BUSCA DE DDE """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*              SELECT SINGLE * INTO wa_zdde_aplicacao
*                FROM zdde_aplicacao
*               WHERE id_registro_expo EQ wa_zreg_exportacao-id_registro_expo.
*
*              "Achou Aplicação de RE
*              IF sy-subrc IS INITIAL.
*
*                SELECT SINGLE * INTO wa_zdde
*                  FROM zdde
*                 WHERE id_dde EQ wa_zdde_aplicacao-id_dde.
*
*                "Achou DDE
*                IF sy-subrc IS INITIAL.
*                  MOVE: wa_zdde-nr_dde       TO wa_zlft_exportacao-num_desp_export,
*                        wa_zdde-nr_valor     TO wa_zlft_exportacao-vlr_desp_export.
*                  WRITE: wa_zdde-dt_dde       TO wa_zlft_exportacao-dt_declaracao,
*                         wa_zdde-dt_averbacao TO wa_zlft_exportacao-dt_averbacao_dec.
*                ELSE.
*                  wa_messagem-tipo = 'W'.
*                  CONCATENATE 'COMEX - Não DDE da(s) aplicação para a RE' wa_zdoc_exp-nr_registro_expo '!' INTO wa_messagem-menssagem SEPARATED BY space.
*                  APPEND wa_messagem TO it_messagem.
*                ENDIF.
*                """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*              ELSE.
*                wa_messagem-tipo = 'W'.
*                CONCATENATE 'COMEX - Não encontrado aplicações para a RE' wa_zdoc_exp-nr_registro_expo '!' INTO wa_messagem-menssagem SEPARATED BY space.
*                APPEND wa_messagem TO it_messagem.
*              ENDIF.
*
*            ELSE.
*              wa_messagem-tipo = 'W'.
*              CONCATENATE 'COMEX - RE' wa_zdoc_exp-nr_registro_expo 'não encontrada em tabela de interface!' INTO wa_messagem-menssagem SEPARATED BY space.
*              APPEND wa_messagem TO it_messagem.
*            ENDIF.
*
*            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*            "" BUSCA B/L """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*            SELECT * INTO TABLE it_zdoc_rem_bl
*              FROM zdoc_rem_bl
*             WHERE id_doc_exp EQ wa_zdoc_exp-id_doc_exp.
*
*            IF NOT sy-subrc IS INITIAL.
*              wa_messagem-tipo = 'W'.
*              CONCATENATE 'COMEX - B/L não encontrada em tabela de interface!' 'Remessa:' wa_vbrp-vgbel INTO wa_messagem-menssagem SEPARATED BY space.
*              APPEND wa_messagem TO it_messagem.
*            ENDIF.
*
*            LOOP AT it_zdoc_rem_bl INTO wa_zdoc_rem_bl.
*              SELECT SINGLE * INTO wa_znom_conhec
*                FROM znom_conhec
*               WHERE id_conhec EQ wa_zdoc_rem_bl-id_conhec.
*              IF NOT sy-subrc IS INITIAL.
*                CONTINUE.
*              ENDIF.
*              IF wa_zlft_exportacao-pais_dest_export IS INITIAL.
*                vg_land1 = wa_znom_conhec-sg_pais_destino.
*
*                SELECT SINGLE * INTO wa_zpais
*                  FROM zpais
*                 WHERE land1 EQ wa_znom_conhec-sg_pais_destino.
*                IF sy-subrc IS INITIAL.
*
*                  wa_zlft_exportacao-pais_dest_export = wa_zpais-bacen.
*                ENDIF.
*              ENDIF.
*
*              wa_zlft_exportacao-num_conhec_emb  = wa_znom_conhec-nr_conhec.
*              wa_zlft_exportacao-tp_conhecimento = wa_znom_conhec-ds_tipo.
*              WRITE wa_znom_conhec-dt_data TO wa_zlft_exportacao-data_emb_export.
*            ENDLOOP.
*            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*          ELSE.
*            wa_messagem-tipo      = 'W'.
*            CONCATENATE 'Nota Fiscal não possui vinculos na transação ZSDT0014 para a remessa' wa_vbrp-vgbel INTO wa_messagem-menssagem SEPARATED BY space.
*            APPEND wa_messagem TO it_messagem.
*          ENDIF.
*        ELSE.
*          wa_messagem-tipo      = 'W'.
*          CONCATENATE 'Fatura' vg_refkey 'não localizada!' INTO wa_messagem-menssagem SEPARATED BY space.
*          APPEND wa_messagem TO it_messagem.
*        ENDIF.
*      ELSE.
*        wa_messagem-tipo      = 'W'.
*        wa_messagem-menssagem = 'Nota fiscal não possui fatura!'.
*        APPEND wa_messagem TO it_messagem.
*      ENDIF.
*
*    ELSE.
*      wa_messagem-tipo      = 'W'.
*      CONCATENATE 'Não foi previsto processos' wa_j_1bnflin-reftyp INTO wa_messagem-menssagem SEPARATED BY space.
*      APPEND wa_messagem TO it_messagem.
*    ENDIF.
*
*    wa_zlft_exportacao-cod_moeda_export = 'USD'.
*    wa_zlft_exportacao-usuario          = sy-uname.
*    wa_zlft_exportacao-horatual         = sy-uzeit.
*
*    WRITE: sy-datlo TO wa_zlft_exportacao-datatual,
*           sy-datum TO wa_zlft_exportacao-dt_registro.
*
*    wa_messagem-tipo      = 'S'.
*    wa_messagem-menssagem = 'Informações de exportação gravada!'.
*    APPEND wa_messagem TO it_messagem.
*
*    MODIFY zlft_exportacao FROM wa_zlft_exportacao.
*
*  ENDLOOP.
*
*  IF NOT it_zlft_saida[] IS INITIAL.
*    COMMIT WORK.
*  ENDIF.
*
*
*  SET TITLEBAR 'LISTAPROC'.
*
*  NEW-PAGE LINE-SIZE 164 LINE-COUNT 40.
*
*  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*
*  LOOP AT it_messagem INTO wa_messagem.
*
*    CASE wa_messagem-tipo.
*      WHEN 'E'.
*        WRITE /001 icon_red_light  AS ICON.
*      WHEN 'W'.
*        WRITE /001 icon_yellow_light  AS ICON.
*      WHEN 'S'.
*        WRITE /001 icon_green_light  AS ICON.
*    ENDCASE.
*
*    WRITE: 006 wa_messagem-cod_holding,
*           014 wa_messagem-cod_matriz,
*           021 wa_messagem-cod_filial,
*           028 wa_messagem-num_nf,
*           040 wa_messagem-cod_cfop_legal,
*           046 wa_messagem-serie,
*           052 wa_messagem-dt_emissao,
*           065 wa_messagem-docnum,
*           076 wa_messagem-ds_nome_transpor,
*           127 wa_messagem-menssagem.
*
*  ENDLOOP.
*
*  ULINE.
*
**&---------------------------------------------------------------------*
**&      TOP-OF-PAGE
**&---------------------------------------------------------------------*
*TOP-OF-PAGE.
*
*  ULINE.
*
*  FORMAT COLOR COL_HEADING INTENSIFIED ON.
*
*  WRITE:/001 'Tipo',
*         006 'Holding',
*         014 'Matriz',
*         021 'Filial',
*         028 'Nota Fiscal',
*         040 'CFOP',
*         046 'Série',
*         052 'Data emissão',
*         065 'Docnum',
*         076 'Navio',
*         127 'Mensagem'.
*
*  ULINE.

*&---------------------------------------------------------------------*
*&      Form  ZALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zalv.
  perform alv_preenche_cat using:
      'COD_HOLDING'        'COD_HOLDING'       '15'       ' '  ' '     ' ' ,
      'COD_MATRIZ'         'COD_MATRIZ'        '15'       ' '  ' '     ' ' ,
      'COD_FILIAL'         'COD_FILIAL'        '15'       ' '  ' '     ' ' ,
      'SERIE'              'SERIE'             '15'       ' '  ' '     ' ' ,
      'SUBSERIE'           'SUBSERIE'          '15'       ' '  ' '     ' ' ,
      'NUM_NF'             'NUM_NF'            '15'       ' '  ' '     ' ' ,
      'DT_EMISSAO'         'DT_EMISSAO'        '15'       ' '  'X'     ' ' ,
      'COD_CFOP_LEGAL'     'COD_CFOP_LEGAL'    '15'       ' '  'X'     ' ' ,
      'COD_MUNICIPIO'      'COD_MUNICIPIO'     '15'       ' '  'X'     ' ' ,
      'NUM_RE_EXPORT'      'NUM_RE_EXPORT'     '15'       ' '  'X'     ' ' ,
      'NUM_DESP_EXPORT'    'NUM_DESP_EXPORT'   '15'       ' '  'X'     ' ' ,
      'NUM_MEMO_EXPORT'    'NUM_MEMO_EXPORT'   '15'       ' '  'X'     ' ' ,
      'USUARIO'            'USUARIO'           '15'       ' '  'X'     ' ' ,
      'DATATUAL'           'DATATUAL'          '15'       ' '  'X'     ' ' ,
      'HORATUAL'           'HORATUAL'          '15'       ' '  'X'     ' ' ,
      'DATA_EMB'           'DATA_EMB_EXPORT'   '15'       ' '  'X'     ' ' ,
      'COD_MOEDA_EXPORT'   'COD_MOEDA_EXPORT'  '15'       ' '  'X'     ' ' ,
      'VLR_DESP_EXPORT'    'VLR_DESP_EXPORT'   '15'       ' '  'X'     ' ' ,
      'PAIS_DEST'          'PAIS_DEST_EMB'     '15'       ' '  ''     ' ' ,
*      'BACEN'              'BACEN'             '15'       ' '  'X'     ' ' ,
      "'DT_DOCUMENTO'       'DT_DOCUMENTO'      '15'       ' '  'X'     ' ' ,
      'VLR_RE_EXPORT'      'VLR_RE_EXPORT'     '15'       ' '  'X'     ' ' ,
      'DT_DECLARACAO'      'DT_DECLARACAO'     '15'       ' '  'X'     ' ' ,
      'DT_REGISTRO'        'DT_REGISTRO'       '15'       ' '  'X'     ' ' ,
      'NUM_CONHEC'         'NUM_CONHECIMENTO'  '15'       ' '  'X'     ' ' ,
      'TP_CONHECT'         'TP_CONHECIMENTO'   '15'       ' '  'X'     ' ' ,
      'DT_AVERBACAO_DEC'   'DT_AVERBACAO_DEC'  '15'       ' '  'X'     ' ' .

endform.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
form alv_preenche_cat  using   p_campo type c
                               p_desc  type c
                               p_tam   type c
                               p_hot   type c
                               p_zero  type c
                               p_mask  type c.
  data: wl_fcat type lvc_s_fcat.

  wl_fcat-tabname   = 'T_ZLFT_EXPORTACAO'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-edit_mask = p_mask.
  wl_fcat-outputlen = p_tam.

  "wl_fcat-convexit  = p_mask.
  append wl_fcat to it_fcat.

endform.                    " ALV_PREENCHE_CAT


*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module z_status output.
  data: wa_fcode type sy-ucomm,
        it_fcode like table of wa_fcode.

  set pf-status 'FF0100' excluding it_fcode.
  set titlebar  'TB0100'.
endmodule.                    "z_status OUTPUT

class lcl_event_receiver definition deferred.

data: wa_event       type ref to  lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_receiver definition.

  public section.
    methods:
            zm_handle_hotspot for event hotspot_click of cl_gui_alv_grid
            importing e_row_id
                      e_column_id
                      es_row_no,

            zm_handle_toolbar for event toolbar of cl_gui_alv_grid
            importing
                e_object e_interactive,

            zm_handle_user_command for event user_command of cl_gui_alv_grid
            importing
                 e_ucomm.
endclass.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

  method: zm_handle_hotspot.
*    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
*                                      E_COLUMN_ID
*                                      ES_ROW_NO.
  endmethod.                    "zm_handle_hotspot


  method zm_handle_toolbar.
*   Incluindo Botão ALV
    perform z_handle_toolbar using e_object
                                   e_interactive.
  endmethod.                    "zm_handle_toolbar

  method zm_handle_user_command.
*   User Command Botões Incluidos

    "PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  endmethod.                    "zm_handle_user_command

endclass.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

form z_handle_toolbar  using    p_object  type ref to cl_alv_event_toolbar_set
                                p_interactive type char1 .

** Constants for button type
  constants:
        c_button_normal           type i value 0        ,
        c_menu_and_default_button type i value 1        ,
        c_menu                    type i value 2        ,
        c_separator               type i value 3        ,
        c_radio_button            type i value 4        ,
        c_checkbox                type i value 5        ,
        c_menu_entry              type i value 6        .

  data sl_toolbar type stb_button.

* Append Seperator
  move c_separator  to sl_toolbar-butn_type.
  append sl_toolbar to p_object->mt_toolbar.



endform.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module z_exibe_alv output.

  if wa_cont is initial.

    create object wa_cont
      exporting
        container_name              = 'CC_ALV'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
  endif.
  if wa_alv is initial and not
    wa_cont is initial.

    create object wa_alv
      exporting
        i_parent          = wa_cont
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.
  endif.

  if wa_event is initial.

    create object wa_event.
    set handler: wa_event->zm_handle_hotspot for wa_alv.
    set handler: wa_event->zm_handle_toolbar for wa_alv.
    set handler: wa_event->zm_handle_user_command for wa_alv.

  endif.

  wa_layout-sel_mode = 'A'.

  call method wa_alv->set_table_for_first_display
    exporting
      is_layout                     = wa_layout
      is_variant                    = gs_variant_c
      i_save                        = 'A'
    changing
      it_outtab                     = t_zlft_exportacao
      it_fieldcatalog               = it_fcat
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  check not wa_alv is initial.
endmodule.                 " Z_EXIBE_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module z_user_command input.
  if sy-dynnr eq '0100'.
    case sy-ucomm.
      when 'BACK' or
           'CANC' or
           'EXIT'  .
        leave to screen 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    endcase.
  endif.
endmodule.                 " Z_USER_COMMAND  INPUT

form zprocessa_dados_due.
  data: vl_data_emb type c length 10.

  sort: t_znom_transporte by id_nomeacao_tran,
        t_zdoc_exp        by id_registro_expo,
        t_zdoc_rem_bl     by id_doc_exp,
        t_znom_conhec     by id_nomeacao_tran ,
        t_vbrp            by vgbel,
        t_j_1bnflin       by refkey,
        t_j_1bnfdoc       by docnum,
        t_zsdt0174        by id_due.

  loop at t_zsdt0170 into wa_zsdt0170.

    CLEAR: wa_zlft_exportacao,
           wa_zsdt0174.

    write: wa_zsdt0170-dt_registro_portal  to wa_zlft_exportacao-dt_declaracao,
           wa_zsdt0170-dt_situacao         to wa_zlft_exportacao-dt_averbacao_dec.

    wa_zlft_exportacao-num_desp_export = 0.

    "move:  wa_zsdt0170-valor     to wa_zlft_exportacao-vlr_desp_export.

    read table t_zsdt0174 INTO wa_zsdt0174 with key id_due = wa_zsdt0170-id_due binary search.

    write: wa_zsdt0170-dt_registro_portal to wa_zlft_exportacao-dt_registro.

    loop at t_zdoc_exp into wa_zdoc_exp where id_due = wa_zsdt0170-id_due.

      CLEAR: wa_vbrp, wa_j_1bnflin, wa_j_1bnfdoc.

      wa_zlft_exportacao-num_re_export    = wa_zdoc_exp-numero_due.

      replace all occurrences of regex '[^0-9]' in wa_zdoc_exp-numero_due with '' ignoring case.

      "read table t_zdoc_memorando into wa_zdoc_memorando with key nr_re = wa_zdoc_exp-nr_registro_expo.
      "if sy-subrc is initial.
      "  wa_zlft_exportacao-num_memo_export = wa_zdoc_memorando-numero_memo.
      "endif.

      read table t_vbrp into wa_vbrp with key vgbel = wa_zdoc_exp-vbeln binary search.
      move : wa_vbrp-netwr     to wa_zlft_exportacao-vlr_re_export.

      read table t_j_1bnflin into wa_j_1bnflin with key refkey = wa_vbrp-vbeln binary search.
      wa_zlft_exportacao-cod_cfop_legal   = wa_j_1bnflin-cfop(4).

      read table t_j_1bnfdoc into wa_j_1bnfdoc with key docnum = wa_j_1bnflin-docnum binary search.
      call function 'J_1B_BRANCH_DETERMINE'
        exporting
          plant                    = wa_j_1bnfdoc-branch
        importing
          txjcd                    = vg_txjcd
        exceptions
          branch_not_found         = 1
          plant_not_found          = 2
          valuation_area_not_found = 3
          company_not_found        = 4
          others                   = 5.

      wa_zlft_exportacao-cod_municipio    = vg_txjcd+3(10).

      concatenate wa_j_1bnfdoc-docdat+6(2) '.' wa_j_1bnfdoc-docdat+4(2) '.' wa_j_1bnfdoc-docdat(4) into wa_zlft_exportacao-dt_emissao.
      wa_zlft_exportacao-cod_holding      = 'MAGGI'.
      wa_zlft_exportacao-cod_matriz       = wa_j_1bnfdoc-bukrs.
      wa_zlft_exportacao-cod_filial       = wa_j_1bnfdoc-branch.
      wa_zlft_exportacao-serie            = wa_j_1bnfdoc-series.
      wa_zlft_exportacao-subserie         = wa_j_1bnfdoc-subser.
      if wa_j_1bnfdoc-nfe eq 'X'.
        wa_zlft_exportacao-num_nf = wa_j_1bnfdoc-nfenum.
      else.
        wa_zlft_exportacao-num_nf = wa_j_1bnfdoc-nfnum.
      endif.

      clear: wa_zlft_exportacao-pais_dest,
             wa_zlft_exportacao-num_conhec,
             wa_zlft_exportacao-tp_conhect,
             wa_zlft_exportacao-data_emb.

      loop at t_znom_conhec into wa_znom_conhec where id_nomeacao_tran eq wa_zdoc_exp-id_nomeacao_tran.
        clear wa_zpais.

        read table t_zpais into wa_zpais with key land1 = wa_zsdt0174-destino_country.
        if sy-subrc is initial.
          wa_zlft_exportacao-pais_dest = wa_zpais-bacen(3).
          wa_zlft_exportacao-pais_dest_export = wa_zlft_exportacao-pais_dest.
        endif.

        if wa_zlft_exportacao-num_conhec is not initial.
          concatenate wa_zlft_exportacao-num_conhec wa_znom_conhec-nr_conhec into wa_zlft_exportacao-num_conhec separated by ','.
        else.
          wa_zlft_exportacao-num_conhec = wa_znom_conhec-nr_conhec.
        endif.

        replace all occurrences of '/' in wa_znom_conhec-ds_tipo with ''.
        condense wa_znom_conhec-ds_tipo no-gaps.
        if sy-subrc is not initial.
          wa_zlft_exportacao-tp_conhect = wa_znom_conhec-ds_tipo.
        endif.

        concatenate wa_znom_conhec-dt_data+6(2) wa_znom_conhec-dt_data+4(2) wa_znom_conhec-dt_data(4) into vl_data_emb separated by '.'.
        if wa_zlft_exportacao-data_emb is not initial.
          concatenate wa_zlft_exportacao-data_emb vl_data_emb into wa_zlft_exportacao-data_emb   separated by ','.
        else.
          wa_zlft_exportacao-data_emb = vl_data_emb.
        endif.
      endloop.

      read table t_zpais into wa_zpais with key land1 = wa_zsdt0174-destino_country.
      if sy-subrc is initial.
        wa_zlft_exportacao-pais_dest = wa_zpais-bacen(3).
        wa_zlft_exportacao-pais_dest_export = wa_zlft_exportacao-pais_dest.
      endif.

      read table t_znom_conhec into wa_znom_conhec with key id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.
      if sy-subrc is initial.
        replace all occurrences of '/' in wa_znom_conhec-ds_tipo with ''.
        wa_zlft_exportacao-tp_conhecimento  = wa_znom_conhec-ds_tipo.
        wa_zlft_exportacao-num_conhec_emb   = wa_znom_conhec-nr_conhec.
      endif.

      wa_zlft_exportacao-cod_moeda_export = 'USD'.
      wa_zlft_exportacao-usuario          = sy-uname.
      wa_zlft_exportacao-horatual         = sy-uzeit.

      write: sy-datlo to wa_zlft_exportacao-datatual.

      modify zlft_exportacao from wa_zlft_exportacao.

      append wa_zlft_exportacao to t_zlft_exportacao.

    endloop.


  endloop.

  commit work.

endform.                    "zprocessa_dados_due.
