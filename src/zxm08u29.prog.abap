*&---------------------------------------------------------------------*
*&  Include           ZXM08U29
*&---------------------------------------------------------------------*
define f_preencher_dynpro.
  clear: tl_bdc.
  move &1 to tl_bdc-dynbegin.
  if &1 = 'X'.
    move:
     &2  to tl_bdc-program,
     &3  to tl_bdc-dynpro.
  else.
    move:
      &2 to tl_bdc-fnam,
      &3 to tl_bdc-fval.
  endif.
  append tl_bdc.
end-of-definition.

field-symbols: <vorgang> type any.
data: t_ekko type ekko,
      t_konv type konv,
      t_ekbe type ekbe occurs 0 with header line,
      vg_tabix type sy-tabix,
      wa_mmcr_tdrseg type mmcr_drseg.

loop at tab_drseg into wa_mmcr_tdrseg where bschl eq '31'.
  vg_tabix = sy-tabix.
  wa_mmcr_tdrseg-zuonr = wa_mmcr_tdrseg-ebeln.
  modify tab_drseg index vg_tabix from wa_mmcr_tdrseg transporting zuonr.
endloop.

data: xtot_deb type ekbe-dmbtr.
**************************************************************************
**************************************************************************
**************************************************************************
** Eduardo Ruttkowski
* Código comentado 28.01.2008 pois ainda nao foi transportado para QA
* e necessita de correção a EXIT referente a centro do pedido
**************************************************************************
**************************************************************************
**************************************************************************
**INI - CESAR COELHO - 07.11.2008
*DATA: V_BUDAT LIKE EKBE-BUDAT,
*      v_ebeln LIKE ekbe-ebeln,
*      v_knttp LIKE ekpo-knttp.
*
data: v_belnr like ekbe-belnr,
      v_gjahr like ekbe-gjahr,
      v_docnum like j_1bnflin-docnum,
      v_refkey like j_1bnflin-refkey,
      wa_zib_nfe_forn    type zib_nfe_forn.

*CLEAR: V_BELNR, V_BUDAT.
*
**break abap.
get parameter id 'RBN' field v_belnr.
get parameter id 'GJR' field v_gjahr.

*
*SELECT SINGLE EBELN BUDAT
*  FROM EKBE
*  INTO (v_ebeln, v_budat)
*  WHERE BELNR EQ V_BELNR AND
*        VGABE EQ '2'.
*
*IF TAB_DRSEG-BUDAT NE V_BUDAT.
*
*  SELECT SINGLE knttp
*  FROM ekpo
*  INTO v_knttp
*  WHERE ebeln EQ v_ebeln.
*
*  IF v_knttp EQ 'A'.
*    message id '00' type 'E' number '398'
*    with 'Para estorno de imobilizado a data do estorno'
*         'tem que ser igual a do lançamento.'.
*  ENDIF.
*ENDIF.
*
**FIM - CESAR COELHO - 07.11.2008


data: v_gsber like invfo-gsber .
clear: v_gsber.

*ALRS 18.10.2013
**Checar se IVA é permitido para a Empresa
if sy-tcode = 'MIRO'.
  data wa_zmmt0043 type zmmt0043.

* US #179390 - MMSILVA - 21.05.2025 - Inicio
  data: ls_regio type t001w-regio.

  select single regio from t001w into ls_regio where werks = tab_drseg-gsber.
* US #179390 - MMSILVA - 21.05.2025 - Fim

  select single *
    from zmmt0043
    into wa_zmmt0043
    where bukrs	=	tab_drseg-bukrs
    and   mwskz = tab_drseg-mwskz
    and   regio = ls_regio. "US #179390 - MMSILVA - 21.05.2025
* US #179390 - MMSILVA - 21.05.2025 - Inicio
  if wa_zmmt0043 is initial.
    select single *
    from zmmt0043
    into wa_zmmt0043
    where bukrs	=	tab_drseg-bukrs
    and   mwskz = tab_drseg-mwskz
    and   regio = ''.
  endif.
* US #179390 - MMSILVA - 21.05.2025 - Fim

  if wa_zmmt0043 is initial. "US #179390 - MMSILVA - 21.05.2025 - Alterado de "if sy-subrc ne 0" para "if wa_zmmt0043 is initial"
    message e000(z01) with 'O IVA escolhido não é permitido '
                           'para esta compra, procurar o Dpto.Fiscal'.

  endif.

endif.

*** Modificação - Eduardo Ruttkowski - 28.01.2008 >>> BEGIN
* qnd utilizar a transação MRRL, verificar direto na tabela EKPO

if sy-tcode = 'MRRL'.
  select single werks from ekpo into v_gsber
    where ebeln = tab_drseg-ebeln and
          ebelp = tab_drseg-ebelp.
  if sy-subrc = 0.

******    Início - Comentar o erro a pedido do Marcos Santos - 27/07/2010
******    IF v_gsber NE tab_drseg-werks.
******      MESSAGE e000(z01) WITH 'Divisão informada diferente do Centro do Pedido.'.
******    ENDIF.
******    Fim - Comentar o erro a pedido do Marcos Santos - 27/07/2010

  endif.
elseif sy-tcode ne 'MR8M'.
*** Modificação - Eduardo Ruttkowski - 28.01.2008 <<< END
  get parameter id 'GSB' field v_gsber.

******    Início - Comentar o erro a pedido do Marcos Santos - 27/07/2010
******  IF v_gsber NE tab_drseg-werks.
******    MESSAGE e000(z01) WITH 'Divisão informada diferente do Centro do Pedido.'.
******  ENDIF.
******    Fim - Comentar o erro a pedido do Marcos Santos - 27/07/2010

** Igor Vilela - Inclusao de SHDB  FB08 - Inicio.
elseif sy-tcode eq 'MR8M'.
  types: begin of ty_rbkp,
          belnr type rbkp-belnr,
          gjahr type rbkp-gjahr,
          bukrs type rbkp-bukrs,
          zuonr type rbkp-zuonr,
          xblnr type rbkp-xblnr,
         end of ty_rbkp,

         begin of ty_bsis,
           bukrs type bsis-bukrs,
           gjahr type bsis-gjahr,
           hkont type bsis-hkont,
           xblnr type bsis-xblnr,
           zuonr type bsis-zuonr,
           belnr type bsis-belnr,
         end of ty_bsis.

  data: wl_rbkp type ty_rbkp,
        tl_bsis type table of ty_bsis with header line,
        tl_bdc            type table of bdcdata with header line,
        wl_par type ctu_params.

  data: wl_shdbnr type zshdbt0001-shdbnr.

  field-symbols: <fs_belnr> type any.
  field-symbols: <fs_gjahr> type any.

  assign ('(SAPLMR1M)RBKPV-BELNR') to <fs_belnr>.
  assign ('(SAPLMR1M)RBKPV-GJAHR') to <fs_gjahr>.

  if <fs_belnr> is assigned
  and <fs_gjahr> is assigned.
    select single belnr gjahr bukrs zuonr xblnr
      from rbkp
      into  wl_rbkp
       where belnr eq <fs_belnr>
         and gjahr eq <fs_gjahr>.

    if sy-subrc is initial.
      select bukrs gjahr hkont xblnr zuonr belnr
        from bsis
        into table tl_bsis
         where bukrs eq wl_rbkp-bukrs
           and gjahr eq wl_rbkp-gjahr
           and hkont eq '0000121401'
           and xblnr eq wl_rbkp-zuonr
           and zuonr eq wl_rbkp-xblnr.

      if sy-subrc is initial.
        wl_par-dismode = 'N'.
        wl_par-updmode = 'A'.
        wl_par-defsize = 'X'.
        wl_par-racommit = 'X'.
        wl_par-nobinpt = 'X'.

        loop at tl_bsis.
          refresh: tl_bdc.
          f_preencher_dynpro:
          'X' 'SAPMF05A'            '0105',
          ' ' 'RF05A-BELNS'         tl_bsis-belnr,
          ' ' 'BKPF-BUKRS'          wl_rbkp-bukrs,
          ' ' 'RF05A-GJAHS'         tl_bsis-gjahr,  "empresa
          ' ' 'UF05A-STGRD'         '01',
          ' ' 'BDC_OKCODE'          '=BU'.

          call function 'ZSHDB_CRIA_EXECUTA'
            exporting
              tcode  = 'FB08'
              params = wl_par
            importing
              shdbnr = wl_shdbnr
            tables
              t_bdc  = tl_bdc.
        endloop.


      endif .

      " Limpa DOCNUM.
      concatenate <fs_belnr> <fs_gjahr> into v_refkey.
      select single docnum
        from j_1bnflin
        into v_docnum
        where refkey = v_refkey.

      select single *
        from zib_nfe_forn
        into wa_zib_nfe_forn
        where docnum = v_docnum.

      if sy-subrc = 0.
        clear v_docnum.
        update zib_nfe_forn set docnum     = space
                                atualizado = ''
       where nu_chave_cnpj eq wa_zib_nfe_forn-nu_chave_cnpj
       and nu_chave_numero eq wa_zib_nfe_forn-nu_chave_numero
       and nu_chave_serie  eq wa_zib_nfe_forn-nu_chave_serie
       and nu_chave_modelo eq wa_zib_nfe_forn-nu_chave_modelo
       and dt_emissao      eq wa_zib_nfe_forn-dt_emissao
       and branch          eq wa_zib_nfe_forn-branch.
      endif.


    endif.

  endif.
** Igor Vilela - Inclusao de SHDB  FB08 - Fim .
endif.

* Início Alteração Ricardo Furst 29.06.2009
assign: ('(SAPLMR1M)RM08M-VORGANG') to <vorgang>.

" Debito posterior 20.10.2015 - ALRS
if <vorgang> = 3.
  message e000(z01) with 'Débito Posterior '
                         ' ñão permitido diretamente'.
endif.
*IF <vorgang> = 3.
*
*  SELECT SINGLE *
*    FROM ekko
*    INTO t_ekko
*    WHERE ebeln = tab_drseg-ebeln.
*
*  IF sy-subrc = 0.
*
*    IF t_ekko-inco1 <> 'FOB' AND
*       t_ekko-inco1 <> 'FCA'.
*
*      MESSAGE e000(z01) WITH 'Não pode ser lançado Débito Posterior para este' ' pedido não é FOB.'.
*
*    ELSE.
*
*      SELECT SINGLE *
*        FROM konv
*        INTO t_konv
*        WHERE knumv = t_ekko-knumv
*          AND kschl = 'FRT'.
*
*      IF sy-subrc = 0.
*
*        SELECT *
*          FROM ekbe
*          INTO TABLE t_ekbe
*          WHERE ebeln = tab_drseg-ebeln
*            AND vgabe = 3.
*
*        IF sy-subrc = 0.
*
*          LOOP AT t_ekbe.
*
*            IF t_ekbe-shkzg = 'S'.
*
*              xtot_deb = xtot_deb + t_ekbe-dmbtr.
*
*            ELSEIF t_ekbe-shkzg = 'H'.
*
*              xtot_deb = xtot_deb - t_ekbe-dmbtr.
*
*            ENDIF.
*
*          ENDLOOP.
*
*          xtot_deb = xtot_deb + tab_drseg-dmbtr.
*
*          IF xtot_deb > t_konv-kbetr.
*
*            MESSAGE e000(z01) WITH 'Valor informado ultrapassa o previsto no pedido.'.
*
*          ENDIF.
*
*        ELSE.
*
*          IF tab_drseg-dmbtr > t_konv-kbetr.
*
*            MESSAGE e000(z01) WITH 'Valor informado ultrapassa o previsto no pedido.'.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.
*
*ENDIF.
* Fim Alteração Ricardo Furst 29.06.2009
*************>>>>>>> Início Alteração Ricardo Furst.
*TYPES: BEGIN OF ty_bkpf,
*        bukrs TYPE bkpf-bukrs,
*        belnr TYPE bkpf-belnr,
*        gjahr TYPE bkpf-gjahr,
*        awkey TYPE bkpf-awkey,
*      END OF ty_bkpf.
*
*DATA: w_bkpf TYPE bkpf,
*      w_bsik TYPE bsik.
*DATA: v_awkey  TYPE bkpf-awkey,
*      v_dat(10) TYPE c,
*      v_hor(10) TYPE c.
*break abap.
*IF sy-tcode EQ 'MR8M'.
*
*  FIELD-SYMBOLS: <bdc> TYPE table .
*
*  DATA : BEGIN OF it_bdc OCCURS 0.
*          INCLUDE STRUCTURE bdcdata.
*  DATA : END OF it_bdc.
*
*  DATA : BEGIN OF t_leaf OCCURS 0.
*          INCLUDE STRUCTURE setleaf.
*  DATA : END OF t_leaf.
*
*  DATA: w_val TYPE setleaf-valfrom.
*  w_val = sy-uname.
*
*  DATA: opt TYPE ctu_params.
*  opt-dismode = 'N'.
*  opt-nobinpt = 'X'.
*  opt-defsize = 'X'.
*
*  SELECT SINGLE * FROM setleaf INTO t_leaf
*    WHERE setname = 'MR8M' AND
*          valfrom = w_val.
*
*  IF sy-subrc = 0.
*
*    CONCATENATE v_belnr
*                tab_drseg-gjahr
*           INTO v_awkey.
*
*    SELECT SINGLE *
*      INTO w_bkpf
*      FROM bkpf
*      WHERE bukrs EQ tab_drseg-bukrs AND
*            gjahr EQ tab_drseg-gjahr AND
*            awkey EQ v_awkey.
*
*    SELECT SINGLE * FROM bsik INTO w_bsik
*      WHERE bukrs = w_bkpf-bukrs AND
*            gjahr = w_bkpf-gjahr AND
*            belnr = w_bkpf-belnr.
*
*    PERFORM bdc_dynpro      USING 'SAPMF05L' '0100'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'RF05L-GJAHR'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_field       USING 'RF05L-BELNR'
*                                  w_bsik-belnr.
*    PERFORM bdc_field       USING 'RF05L-BUKRS'
*                                  w_bsik-bukrs.
*    PERFORM bdc_field       USING 'RF05L-GJAHR'
*                                  w_bsik-gjahr.
*    PERFORM bdc_dynpro      USING 'SAPMF05L' '0750'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=PI'.
*    PERFORM bdc_dynpro      USING 'SAPMF05L' '0302'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'BSEG-ZLSPR'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=AE'.
*    PERFORM bdc_field       USING 'BSEG-ZLSPR'
*                                  'N'.
*
*
*    ASSIGN: ('(SAPLXM08)bdcdata[]') TO <bdc>.
*
*    CALL TRANSACTION 'FB02'
*      USING <bdc>
*      OPTIONS FROM opt.
**          MESSAGES INTO messtab.
*
*
*  ELSE.
*
*    CONCATENATE v_belnr
*                tab_drseg-gjahr
*           INTO v_awkey.
*
*    SELECT SINGLE *
*      INTO w_bkpf
*      FROM bkpf
*      WHERE bukrs EQ tab_drseg-bukrs AND
*            gjahr EQ tab_drseg-gjahr AND
*            awkey EQ v_awkey.
*
*    IF sy-subrc EQ 0.
*
*      v_dat = sy-datum - w_bkpf-cpudt.
*      v_hor = sy-uzeit - w_bkpf-cputm.
*
*      IF  v_dat >= 2 AND
*               v_hor > 0 .
*
*        MESSAGE e000(z01) WITH 'Estorno não permitido prazo de 48 horas '
*                               '(Data de Entrada da Fatura) ultrapassado '
*                               'entrar em contato com área Fiscal'.
**      STOP.
*
**    ELSEIF v_dat <= 2 AND
**           v_hor <= 0.
*      ELSE.
*        SELECT SINGLE * FROM bsik INTO w_bsik
*          WHERE bukrs = w_bkpf-bukrs AND
*                gjahr = w_bkpf-gjahr AND
*                belnr = w_bkpf-belnr.
*        DATA: w_data TYPE datum,
*              w_dat2 TYPE datum.
*        w_data = w_bsik-zfbdt + w_bsik-zbd1t.
*        v_dat = sy-datum - w_data.
*        w_dat2 = sy-datum + 3.
*        IF w_data < w_dat2.
**        IF NOT ( v_dat >= 3 AND
**                 v_hor > 0      ).
*
*          MESSAGE e000(z01) WITH 'Estorno não permitido prazo de 72 horas '
*                                 'ultrapassado (Data de Vencimento) '
*                                 'entrar em contato com área de Liquidação'.
**        exit.
*
*        ELSE.
*
*          PERFORM bdc_dynpro      USING 'SAPMF05L' '0100'.
*          PERFORM bdc_field       USING 'BDC_CURSOR'
*                                        'RF05L-GJAHR'.
*          PERFORM bdc_field       USING 'BDC_OKCODE'
*                                        '/00'.
*          PERFORM bdc_field       USING 'RF05L-BELNR'
*                                        w_bsik-belnr.
*          PERFORM bdc_field       USING 'RF05L-BUKRS'
*                                        w_bsik-bukrs.
*          PERFORM bdc_field       USING 'RF05L-GJAHR'
*                                        w_bsik-gjahr.
*          PERFORM bdc_dynpro      USING 'SAPMF05L' '0750'.
*          PERFORM bdc_field       USING 'BDC_OKCODE'
*                                        '=PI'.
*          PERFORM bdc_dynpro      USING 'SAPMF05L' '0302'.
*          PERFORM bdc_field       USING 'BDC_CURSOR'
*                                        'BSEG-ZLSPR'.
*          PERFORM bdc_field       USING 'BDC_OKCODE'
*                                        '=AE'.
*          PERFORM bdc_field       USING 'BSEG-ZLSPR'
*                                        'N'.
*
*
*          ASSIGN: ('(SAPLXM08)bdcdata[]') TO <bdc>.
*
*
*          CALL TRANSACTION 'FB02'
*            USING <bdc>
*            OPTIONS FROM opt.
**          MESSAGES INTO messtab.
*
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDIF.
*
**************<<<<<< Fim Alteração Ricardo Furst.
