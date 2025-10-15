"Name: \PR:SAPMV45A\FO:USEREXIT_PRICING_PREPARE_TKOMP\SE:BEGIN\EI
ENHANCEMENT 0 Z_USEXIT_PRICING_PREPARE_TKOMP.


*  TKOMP-zzfield = xxxx-zzfield2.
*BBKO/Vagner Santos - Início da alteração - 16.08.2010
* Confrontar os valores dos impostos e leis da aba "País" do item do
* pedido com a tabela ZSDT0008.
* Tipos
*
  types: begin of y_kna1,
           brsch type kna1-brsch,
           regio type kna1-regio,
           cityc type kna1-cityc,
           kunnr type kna1-kunnr,
         end of y_kna1,

         begin of y_lfa1,
           brsch type lfa1-brsch,
           regio type lfa1-regio,
           lifnr type lfa1-lifnr,
         end of y_lfa1,

         begin of y_t001w,
           regio type t001w-regio,
         end of y_t001w,

         begin of y_mbew,
           mtorg type mbew-mtorg, "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
           ownpr type mbew-ownpr,
         end of y_mbew,

*-CS2025000025-#164218-27.01.2025-JT-inicio
         begin of y_zsdt0008,  "y_zsdt0370, "y_zsdt0008,
           j_1btxsdc  type zsdt0008-j_1btxsdc,
           j_1btaxlw1 type zsdt0008-j_1btaxlw1,
           j_1btaxlw2 type zsdt0008-j_1btaxlw2,
           j_1btaxlw4 type zsdt0008-j_1btaxlw4,
           j_1btaxlw5 type zsdt0008-j_1btaxlw5,
         end of y_zsdt0008.  "y_zsdt0370. "y_zsdt0008.
*-CS2025000025-#164218-27.01.2025-JT-fim

* Tabelas internas
  data: ti_xkomv type table of komv.

* Campos
  data: vl_txt01(50) type c,
        vl_txt02(50) type c,
        vl_txt03(50) type c,
        vl_txt04(50) type c,
        vl_taxlaw    type j_1btxic3-taxlaw,
        vl_shtyp     type zsdt0008-shtyp,  "zsdt0370-shtyp,  "zsdt0008-shtyp, "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vl_tdlnr     type zsdt0008-tdlnr,  "zsdt0370-tdlnr,  "zsdt0008-tdlnr, *-CS2025000025-#164218-27.01.2025-JT-inicio
        vl_branch    type j_1bbranc_,
        vl_kunnr     type vtpa-kunnr,
        vl_lifnr     type vtpa-lifnr.

* Estruturas
  data: st_kna1     type y_kna1,
        st_lfa1     type y_lfa1,
        st_t001w    type y_t001w,
        st_mbew     type y_mbew,
        st_zsdt0008 type y_zsdt0008,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
*       st_zsdt0370 TYPE y_zsdt0370,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
        st_xvbap    type vbapvb,
        st_xkomv    type komv,
        st_vttp     type vttp,
        st_lips     type lips,
        st_vbak_aux type vbak,
        st_vbfa_aux type vbfa,
        st_t001k    type t001k,
        limite      type  netwr,
        maximo      type  netwr,
        minimo      type  netwr,
        ls_knvi     type knvi.

*  DATA: lc_dados   TYPE zsde0183,   "*-CS2025000025-#164218-27.01.2025-JT-inicio
*        lc_retorno TYPE zsdt0370_t. "*-CS2025000025-#164218-27.01.2025-JT-inicio

* Constantes
  constants: c_ibrx     type komv-kschl value 'IBRX',
             c_va01     type sy-tcode   value 'VA01',
             c_va02     type sy-tcode   value 'VA02',
             c_zsdt0044 type sy-tcode   value 'ZSDT0044',
             c_zsdt0062 type sy-tcode   value 'ZSDT0062',
             c_zsdt0066 type sy-tcode   value 'ZSDT0066',
             c_zles0077 type sy-tcode   value 'ZLES0077',
             c_zsdt0087 type sy-tcode   value 'ZSDT0087',
             c_zsdt0081 type sy-tcode   value 'ZSDT0081', "// US-169490 WBARBOSA 18/09/2025
             c_zsdt0112 type sy-tcode   value 'ZSDT0112',
             c_4470     type sy-dynnr   value '4470',   "Tela de intes - Aba País
             c_br(2)    type c          value 'BR',
             c_76(2)    type c          value '76',
             c_78(2)    type c          value '78',     "RIM-SKM-IR120631
             c_79(2)    type c          value '79',
             c_ztro(4)  type c          value 'ZTRO',
             c_ztrt(4)  type c          value 'ZTRT',
             c_ztrh(4)  type c          value 'ZTRH',
             c_lr(2)    type c          value 'LR',
             c_pc(2)    type c          value 'PC'.


  data: gp_externo type extwg.

* Início - Sara Oikawa 18.05.2020 - CS2020000500
  data: gp_matkl        type matkl.
  constants: c_80(2)    type c          value '80'.
* Fim    - Sara Oikawa 18.05.2020 - CS2020000500

*BBKO/Vagner Santos - Início da alteração - 07/10/2010
  constants: c_v        type c          value 'V'.
*BBKO/Vagner Santos - Fim da alteração - 07/10/2010

* Inicio Alteração Conforme solicitado 07/02/2012
  constants: c_77(2)    type c          value '77'.
* Fim Alteração Conforme solicitado 07/02/2012


*  CALL METHOD ZCL_SOLICITACAO_OV=>CHECK_LIMITE_DESCONTO
*    IMPORTING
*      VBAK   = VBAK
*      VBAP   = VBAP
*      KOMV   = XKOMV[]
*    RECEIVING
*      RETURN = DATA(OK).
*
*  IF OK IS NOT INITIAL.
**    MESSAGE E836(SD) WITH VL_TXT01 VL_TXT02 VL_TXT03 VL_TXT04.
*    MESSAGE E836(SD) WITH |Desconto Absoluto fora dos Limites Estabelecido na ZSDT0153!|.
*  ENDIF.


*  IF VBAK-AUART IS NOT INITIAL
* AND VBAK-SPART IS NOT INITIAL
* AND VBAK-VKORG IS NOT INITIAL
* AND VBAK-WAERK IS NOT INITIAL.
*
**    BREAK-POINT.
*    SELECT SINGLE NETWR
*      FROM ZSDT0195
*      INTO LIMITE
*      WHERE AUART  EQ VBAK-AUART
*        AND SPART  EQ VBAK-SPART
*        AND VKORG  EQ VBAK-VKORG
*        AND WAERK  EQ VBAK-WAERK
*        AND STATUS EQ ABAP_FALSE.
*
*    IF SY-SUBRC IS INITIAL.
*
*      DATA(QTD_RB00) =
*      REDUCE INT4( INIT X = 0
*                  FOR L_KOMV IN XKOMV[]
*                WHERE ( KNUMV = VBAK-KNUMV AND KPOSN EQ VBAP-POSNR AND KSCHL EQ 'RB00' )
*                NEXT X = X + 1
*              ).
*
*      IF QTD_RB00 EQ 1.
*
*        DATA(DESCONTO) = XKOMV[ KNUMV = VBAK-KNUMV KPOSN = VBAP-POSNR KSCHL = 'RB00' ]-KBETR.
*
*        MAXIMO = LIMITE.
*        MINIMO = LIMITE * -1.
*
*        IF DESCONTO NOT BETWEEN MINIMO AND MAXIMO.
*          MESSAGE E899 WITH 'Desconto ABS fora do Limite Cadastrado! "XXXXX"'.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.

  if (   sy-tcode = c_zsdt0087 or
         sy-tcode = c_zsdt0044 or
         sy-tcode = 'ZSDT0081' or "// US-169490 WBARBOSA 24/07/2025
         sy-tcode = c_zsdt0112 ).

*     "// Substitui o Vendedor caso não encontre na relação
    "// Também foi necessário criar a ENHANCEMENT "ZSD_ORGDATA_CHECK_01" para tratar essa validação do vendedor.
    if vbak-vkgrp is not initial.
      select count(*)
        from tvbvk
        up to 1 rows
        where vkbur eq vbak-vkbur
          and vkgrp eq vbak-vkgrp.
      if sy-subrc is not initial.
        vbak-vkgrp = '106'.
      endif.
    endif.

  endif.

  if xkomv[] is not initial
  and vbap-vkaus is not initial
  and vbap-vkaus ne c_v.

    if sy-tcode eq c_zsdt0066." OR
*       SY-TCODE EQ C_ZSDT0044.
      data(qtd_linha) =
      reduce int4( init x = 0
                  for l_komv in xkomv[]
                where ( knumv = vbak-knumv and kposn eq vbap-posnr and kschl eq 'RB00' )
                next x = x + 1
              ).

      if qtd_linha gt 1.
        delete xkomv where zaehk eq '01' and kschl eq 'RB00' and kposn eq vbap-posnr.
      endif.
    endif.
  endif.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
  data: lwa_tvarvc type tvarvc.
  data: tcode_no_check type c.

  tcode_no_check = abap_false.
  select single *
    from tvarvc into lwa_tvarvc
   where name eq 'MV45AFZZ_TCODE_NO_CHECK_LFIS'
     and low  eq sy-tcode.
  if sy-subrc eq 0.
    tcode_no_check = abap_true.
  endif.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

  data(obj_tcode) = zcl_memory_temp_single=>get_instance(  ).
  data(lv_tcode) = obj_tcode->get_data( ).

  if lv_tcode is not initial and lv_tcode eq 'ZSDT0081'.
    clear tcode_no_check.
  endif.

  if ( sy-tcode   = c_va01 or
       sy-tcode   = c_va02 or
       t180-tcode = c_va01 or   "// US-169490 WBARBOSA 16/08/2025
       t180-tcode = c_va02 or   "// US-169490 WBARBOSA 16/08/2025
       lv_tcode = c_zsdt0081 or "// US-169490 WBARBOSA 18/09/2025
       sy-tcode = c_zsdt0062 or
       sy-tcode = 'ZMEMO00' or
       sy-tcode = c_zsdt0066 or
       sy-tcode = c_zles0077 or
       sy-tcode = c_zsdt0087 or
       sy-tcode = c_zsdt0081 or  "// US-169490 WBARBOSA 24/07/2025
       sy-tcode = c_zsdt0044 or
       sy-tcode = c_zsdt0112 )
  and tcode_no_check eq abap_false "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
  and xkomv[] is not initial
*BBKO/Vagner Santos - Início da alteração - 07/10/2010
*      vbap-vkaus IS NOT INITIAL. "Este campo é de preenchimento obrigatório
  and vbap-vkaus is not initial "Este campo é de preenchimento obrigatório
  and vbap-vkaus ne c_v. " VKAUS CODIGO DE UTILIZAÇÃO "V" SERVIÇO
*BBKO/Vagner Santos - Fim da alteração - 07/10/2010

    if vbak-auart = c_ztro
    or vbak-auart = c_ztrt
    or vbak-auart = c_ztrh.

      clear: vl_kunnr,vl_lifnr.
      select kunnr into vl_kunnr
                   from vtpa
                up to 1 rows
                  where vbeln = vbak-tknum
                    and parvw = c_lr.
      endselect.
      if sy-subrc eq 0.
* Obter informações do cliente parceiro LR
        select single brsch regio cityc kunnr into st_kna1
                                              from kna1
                                             where kunnr = vl_kunnr.
      endif.
      select lifnr into vl_lifnr
                   from vtpa
                up to 1 rows
                  where vbeln = vbak-tknum
                    and parvw = c_pc.
      endselect.
      if sy-subrc eq 0.
* Obter informações do fornecedor parceiro PC
        select single brsch regio lifnr into st_lfa1
                                              from lfa1
                                             where lifnr = vl_lifnr.
      endif.

    else.
* Obter informações do cliente original
      select single brsch regio cityc kunnr into st_kna1
                                            from kna1
                                           where kunnr = vbak-kunnr.
    endif.
    check sy-subrc eq 0.

* Obter informações do centro
    select single regio into st_t001w
                        from t001w
                       where werks = vbap-werks.
    check sy-subrc eq 0.

    "Comentado codigo abaixo, para publicação dos chamado CS2020001303
*    IF VBAK-AUART EQ 'ZTAB' OR
*       VBAK-AUART EQ 'ZTAF' OR
*       VBAK-AUART EQ 'ZTAG' OR
*       VBAK-AUART EQ 'ZTAM'.
*
*      DATA: LWA_LFA1_T2 TYPE LFA1,
*            LWA_KNA1_T3 TYPE KNA1.
*
*      READ TABLE XVBPA INTO DATA(LWA_XBPA_T2) WITH KEY PARVW = 'T2'.
*      IF ( SY-SUBRC EQ 0 ) AND ( LWA_XBPA_T2-LIFNR IS  NOT INITIAL ).
*        READ TABLE XVBPA INTO DATA(LWA_XBPA_T3) WITH KEY PARVW = 'T3'.
*        IF ( SY-SUBRC EQ 0 ) AND ( LWA_XBPA_T3-KUNNR IS  NOT INITIAL ).
*          SELECT SINGLE *
*            FROM LFA1 INTO LWA_LFA1_T2
*           WHERE LIFNR EQ LWA_XBPA_T2-LIFNR.
*
*          IF SY-SUBRC EQ 0.
*            ST_T001W-REGIO = LWA_LFA1_T2-REGIO.
*          ENDIF.
*
*          SELECT SINGLE *
*            FROM KNA1 INTO LWA_KNA1_T3
*           WHERE KUNNR EQ LWA_XBPA_T3-KUNNR.
*
*          IF SY-SUBRC EQ 0.
*            ST_KNA1-REGIO = LWA_KNA1_T3-REGIO.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    "Comentado codigo acim, para publicação dos chamado CS2020001303

* Obter informações do material.
    select  mtorg ownpr into st_mbew
                 from mbew
            up to 1 rows
                where matnr = vbap-matnr
                  and bwkey = vbap-werks.
    endselect.
    check sy-subrc eq 0.

* Obter o código de imposto para a condição IBRX
    refresh ti_xkomv.
    ti_xkomv[] = xkomv[].
    sort ti_xkomv by kschl.
    read table ti_xkomv into st_xkomv with key kschl = c_ibrx
                      binary search.
    check sy-subrc eq 0.

* Verificar se há alguma regra de exceção dinámica de ICMS

*  "// get grupo de mercadoria externo
    select single extwg matkl
      from mara
      into ( gp_externo, gp_matkl )
      where matnr eq vbap-matnr.

    clear vl_taxlaw.
* Início - Sara Oikawa 18.05.2020 - CS2020000500
* Ajuste para atender as regras de Vendas Futura e Venda Triangular
* Incluir a busca na tabela J_1BTXIC3-GROUP = 80 antes dos demais grupos;
* Se achar nele, não deve buscar nos demais.
    select single taxlaw into vl_taxlaw
                         from j_1btxic3
                        where land1    = c_br
                          and shipfrom = st_t001w-regio
                          and shipto   = st_kna1-regio
                          and gruop    = c_80
                          and value    = st_kna1-kunnr
                          and value2   = vbap-matnr
                          and value3   = gp_matkl.
    if vl_taxlaw is initial.
* Fim - Sara Oikawa 18.05.2020 - CS2020000500
      select single taxlaw into vl_taxlaw
                           from j_1btxic3
                          where land1    = c_br
                            and shipfrom = st_t001w-regio
                            and shipto   = st_kna1-regio
                            and gruop    = c_76
                            and value    = st_kna1-kunnr
                            and value2   = vbap-matnr.
* Início - Sara Oikawa 18.05.2020 - CS2020000500
    endif.
* Fim - Sara Oikawa 18.05.2020 - CS2020000500

* RIM - SKM - IR120631 - Início
    if vl_taxlaw is initial.
      select single taxlaw into vl_taxlaw
                           from j_1btxic3
                          where land1    = c_br
                            and shipfrom = st_t001w-regio
                            and shipto   = st_kna1-regio
                            and gruop    = c_78
                            and value    = gp_externo
                            and value2   = vbap-werks.
    endif.
* RIM - SKM - IR120631 - Fim
* Alteração Conforme solicitação 20.09.2018 - Se não achar gruoup = 79 procura o 76.
    if vl_taxlaw is initial.
      select single taxlaw into vl_taxlaw
                           from j_1btxic3
                          where land1    = c_br
                            and shipfrom = st_t001w-regio
                            and shipto   = st_kna1-regio
                            and gruop    = c_79
                            and value    = gp_externo.
    endif.
* fim

* Alteração Conforme solicitação 07.02.2012 - Se não achar gruoup = 76 procura o 77.
    if vl_taxlaw is initial.
      select single taxlaw into vl_taxlaw
                     from j_1btxic3
                    where land1    = c_br
                      and shipfrom = st_t001w-regio
                      and shipto   = st_kna1-regio
                      and gruop    = c_77
                      and value    = vbap-matnr.

    endif.
* Fim alteração conforme solicitação 07.02.2012

* Buscar Numero TKNUM quando...
    if ( vbak-auart eq 'ZCFH' ) and
*    IF ( ( VBAK-AUART EQ 'ZCFH' ) OR ( VBAK-AUART EQ 'ZSFR' ) ) AND
     ( vbak-vgbel is not initial ) and
     ( vbak-tknum is initial     ).

      select single *
        from vbfa into st_vbfa_aux
       where vbeln   eq vbak-vgbel
         and vbtyp_n eq 'M'
         and vbtyp_v eq 'C'.

      if ( sy-subrc = 0 ) and ( st_vbfa_aux-vbelv is not initial ).
        select single *
          from vbak into st_vbak_aux
         where vbeln eq st_vbfa_aux-vbelv.

        if ( sy-subrc = 0 ) and ( st_vbak_aux-tknum is not initial ).
          vbak-tknum = st_vbak_aux-tknum.
        endif.
      endif.
    endif.

* O usuário já deverá ter informado o número de documento de transporte
* na aba Dados adicionais B
    clear: vl_tdlnr, vl_shtyp, vl_branch.
    select single shtyp tdlnr tplst into (vl_shtyp,vl_tdlnr,vl_branch)
                        from vttk
                       where tknum = vbak-tknum.

* Pega Tomador do serviço frete
    clear st_t001k.
    select single * into st_vttp
                  from vttp
                  where tknum = vbak-tknum.
    if sy-subrc = 0.
      select single * into st_lips
                    from lips
                    where vbeln = st_vttp-vbeln.
      if sy-subrc = 0.
        select single * into st_t001k
                      from t001k
                      where bwkey	=	st_lips-werks.
      endif.
    endif.

*    IF VBAK-AUART EQ 'ZSFR'.
**     "// Resgata os impostos da O.V Original
*      SELECT SINGLE J_1BCFOP J_1BTXSDC J_1BTAXLW1 J_1BTAXLW2 J_1BTAXLW4 J_1BTAXLW5
*        FROM VBAP INTO ( VBAP-J_1BCFOP,  VBAP-J_1BTXSDC, VBAP-J_1BTAXLW1, VBAP-J_1BTAXLW2, VBAP-J_1BTAXLW4, VBAP-J_1BTAXLW5 )
*       WHERE VBELN EQ ST_VBFA_AUX-VBELV
*          AND POSNR EQ ST_VBFA_AUX-POSNV.
*
*      DATA VL_DSTCAT TYPE J_1BDSTCAT.
**     "// get Cliente informado pelo usuario
*      TRY .
*          DATA(CLIENTE)    = XVBPA[ PARVW = 'LR' ]-KUNNR.
*        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*      ENDTRY.
*
**     "// get Fornecedor informado pelo usuario*
*      TRY .
*          DATA(FORNECEDOR) = XVBPA[ PARVW = 'PC' ]-LIFNR.
*        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*      ENDTRY.
*
*
**     "// get Região do Cliente
*      SELECT SINGLE REGIO
*        FROM KNA1
*        INTO ST_KNA1-REGIO
*        WHERE KUNNR  EQ CLIENTE.
**     "// get Região do Fornecedor
*      SELECT SINGLE REGIO
*        FROM LFA1
*        INTO ST_T001W-REGIO
*        WHERE LIFNR EQ FORNECEDOR.
*
**     "// Categoria de Destino
*      VL_DSTCAT = COND #( WHEN ST_T001W-REGIO EQ ST_KNA1-REGIO THEN 0 ELSE 1 ).
*
**      //" Agente de Frete já esta atribuido
**      VL_TDLNR
*
**     "// Empresa
*      VL_BRANCH = |{ VBAK-KUNNR ALPHA = IN }|.
*
*      SELECT COUNT(*)
*       FROM KNA1
*       WHERE KUNNR EQ VBAK-KUNNR
*        AND KTOKD EQ 'ZCIC'.
*      IF SY-SUBRC IS NOT INITIAL.
*        EXIT.
*      ENDIF.
*
**     "// get Local de Negocio categoria CFOP
*      DATA VL_INDUSTRY TYPE J_1BINDUS2.
*      DATA VL_BUKRS TYPE BUKRS.
*      SELECT SINGLE INDUSTRY BUKRS
*        FROM J_1BBRANCH
*        INTO (VL_INDUSTRY,VL_BUKRS)
*       WHERE BRANCH EQ VL_BRANCH.
*
**     "// get CFOP "Código fiscal de Operações e Prestações"
*      SELECT SINGLE CFOP
*        FROM ZLEST0030
*        INTO VBAP-J_1BCFOP
*       WHERE DIRECT     EQ '2'
*         AND DSTCAT     EQ VL_DSTCAT
*         AND INDUSTRY   EQ VL_INDUSTRY
*         AND TPPARCEIRO EQ '0'
*         AND TDLNR      EQ VL_TDLNR
*         AND BUKRS      EQ VL_BUKRS.
*
**     "// IMPOSTOS
**     "// Codigo do IVA
*      ST_XKOMV-MWSK1 = 'SD'.
*
*    ENDIF.


********************************************************************** "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
    data: lr_matnr type range of mara-matnr,
          lr_matkl type range of mara-matkl,
          lr_extwg type range of mara-extwg,
          lr_bukrs type range of t001-bukrs,
          lr_steuc type range of marc-steuc,
          lr_mtorg type range of mbew-mtorg,
          it_mara  type mara,
          it_marc  type marc,
          lv_ktokd type kna1-ktokd,
          lv_bukrs type j_1bbranch-bukrs.

*    APPEND VALUE #( sign = 'I' option = 'EQ' low = vbap-matnr ) TO lr_matnr.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = st_t001k-bukrs ) TO lr_bukrs.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = st_mbew-mtorg ) TO lr_mtorg.

*    IF vbap-matnr IS NOT INITIAL.
*      SELECT SINGLE *
*        INTO it_mara
*        FROM mara
*       WHERE matnr = vbap-matnr.

*      lr_matkl[] = VALUE #( FOR p01 IN it_mara ( option = 'EQ' sign = 'I' low = p01-matkl ) ).
*      SORT lr_matkl.
*      DELETE ADJACENT DUPLICATES FROM lr_matkl.
*
*      lr_extwg[] = VALUE #( FOR p02 IN it_mara ( option = 'EQ' sign = 'I' low = p02-extwg ) ).
*      SORT lr_extwg.
*      DELETE ADJACENT DUPLICATES FROM lr_extwg.

*      SELECT SINGLE *
*        INTO it_marc
*        FROM marc
*       WHERE matnr = vbap-matnr
*         AND werks = vbap-werks.

*      lr_steuc[] = VALUE #( FOR p03 IN it_marc ( option = 'EQ' sign = 'I' low = p03-steuc+0(4) ) ).
*      SORT lr_steuc.
*      DELETE ADJACENT DUPLICATES FROM lr_steuc.

*      "Add vazio
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO lr_bukrs.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO lr_mtorg.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO lr_steuc.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO lr_matnr.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO lr_matkl.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO lr_extwg.
*  ENDIF.

**********************************************************************

* Selecionar os códigos dos direitos fiscais e leis na tabela local.
*-CS2025000025-#164218-27.01.2025-JT-inicio
    select single j_1btxsdc j_1btaxlw1 j_1btaxlw2
                  j_1btaxlw4 j_1btaxlw5
                  into st_zsdt0008
                  from zsdt0008
                 where auart      = vbak-auart
                   and vkaus      = vbap-vkaus
                   "AND BRSCH      = ST_LFA1-BRSCH
                   and uf_centro  = st_t001w-regio
                   and uf_cliente = st_kna1-regio
                   "AND CITYC      = ST_KNA1-CITYC
                   and mwsk1      = st_xkomv-mwsk1
                   and ownpr      = st_mbew-ownpr
                   and tdlnr      = vl_tdlnr
                   and bukrs      = st_t001k-bukrs. "Tomador
*                  AND empresa    IN lr_bukrs "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
*                  AND mtorg      IN lr_mtorg "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
*                  AND steuc      IN lr_steuc "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
*                  AND matnr      IN lr_matnr "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
*                  AND matkl      IN lr_matkl "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
*                  AND extwg      IN lr_extwg "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA

*    lc_dados-auart-valor      = vbak-auart.
*    lc_dados-vkaus-valor      = vbap-vkaus.
*    lc_dados-mwsk1-valor      = st_xkomv-mwsk1.
*    lc_dados-uf_centro-valor  = st_t001w-regio.
*    lc_dados-uf_cliente-valor = st_kna1-regio.
*    lc_dados-ownpr-valor      = st_mbew-ownpr.
*    lc_dados-tdlnr-valor      = vl_tdlnr.
**   lc_dados-bukrs_toma-valor = st_t001k-bukrs.
*    lc_dados-bukrs_emit-valor = st_t001k-bukrs.
*    lc_dados-kunnr-valor      = vbak-kunnr.
*    lc_dados-werks-valor      = vbap-werks.
*    lc_dados-matnr-valor      = vbap-matnr.
*
*    lc_retorno = zcl_impostos=>get_tax_imposto( i_dados = lc_dados ).
*
*    READ TABLE lc_retorno INTO DATA(wc_retorno) INDEX 1.
*    IF sy-subrc = 0.
*      MOVE-CORRESPONDING wc_retorno  TO st_zsdt0370.
*    ENDIF.
*-CS2025000025-#164218-27.01.2025-JT-fim

    if sy-subrc ne 0.           "*-CS2025000025-#164218-27.01.2025-JT-fim
*   IF lc_retorno[] IS INITIAL. "*-CS2025000025-#164218-27.01.2025-JT-fim
      vl_txt01 = 'Parametrizar Leis Fiscais->'.
      concatenate  vbak-auart '/'
                   vbap-vkaus '/_/'
                   st_t001w-regio '/'
                   st_kna1-regio '/_/'
                   st_xkomv-mwsk1 '/'
                   st_mbew-ownpr '/'
                   '/_/'
                   vl_tdlnr '/'
                   st_t001k-bukrs  into vl_txt02.

      vl_txt03 = 'Procurar Departamento Fiscal tributário.'.
*      IF sy-uname = 'abap'.
      message e836(sd) with vl_txt01 vl_txt02 vl_txt03 vl_txt04.
*      MESSAGE w836(sd) WITH vl_txt01 vl_txt02 vl_txt03 vl_txt04.
*      ENDIF.
    elseif sy-dynnr ne c_4470.                             "Aba País
      if vbap-j_1btxsdc  ne st_zsdt0008-j_1btxsdc. "st_zsdt0370-j_1btxsdc. "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vbap-j_1btxsdc = st_zsdt0008-j_1btxsdc.    "st_zsdt0370-j_1btxsdc.    "*-CS2025000025-#164218-27.01.2025-JT-inicio
      endif.
      if ( vl_taxlaw is not initial ) and ( vbak-auart ne 'ZTER' ) and not ( vbak-auart eq 'ZFUT' and st_t001w-regio eq 'MT' ).
        vbap-j_1btaxlw1 = vl_taxlaw.
        st_zsdt0008-j_1btaxlw1 = vl_taxlaw.             "st_zsdt0370-j_1btaxlw1 = vl_taxlaw.         "*-CS2025000025-#164218-27.01.2025-JT-inicio
      elseif vbap-j_1btaxlw1 ne st_zsdt0008-j_1btaxlw1.  "st_zsdt0370-j_1btaxlw1.
        vbap-j_1btaxlw1 = st_zsdt0008-j_1btaxlw1.   "st_zsdt0370-j_1btaxlw1.   "*-CS2025000025-#164218-27.01.2025-JT-inicio
      endif.
      if vbap-j_1btaxlw2 ne st_zsdt0008-j_1btaxlw2. "st_zsdt0370-j_1btaxlw2. "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vbap-j_1btaxlw2 = st_zsdt0008-j_1btaxlw2.   "st_zsdt0370-j_1btaxlw2.   "*-CS2025000025-#164218-27.01.2025-JT-inicio
      endif.
      if vbap-j_1btaxlw4 ne st_zsdt0008-j_1btaxlw4. "st_zsdt0370-j_1btaxlw4. "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vbap-j_1btaxlw4 =  st_zsdt0008-j_1btaxlw4.  "st_zsdt0370-j_1btaxlw4.  "*-CS2025000025-#164218-27.01.2025-JT-inicio
      endif.
      if vbap-j_1btaxlw5 ne st_zsdt0008-j_1btaxlw5.  "st_zsdt0370-j_1btaxlw5. "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vbap-j_1btaxlw5 =  st_zsdt0008-j_1btaxlw5.   "st_zsdt0370-j_1btaxlw5.  "*-CS2025000025-#164218-27.01.2025-JT-inicio
      endif.
*BBKO/Vagner Santos - Início da alteração - 07.10.2010
*    ELSEIF ( vbap-j_1btxsdc   NE vl_taxlaw              AND
*            ( vbap-j_1btxsdc   NE vl_taxlaw             AND

    elseif  ( vl_taxlaw is not initial ) and ( vbak-auart ne 'ZTER' ) and not ( vbak-auart eq 'ZFUT' and st_t001w-regio eq 'MT' ).
      if vbap-j_1btaxlw1 ne vl_taxlaw  or
         vbap-j_1btxsdc  ne st_zsdt0008-j_1btxsdc  or "st_zsdt0370-j_1btxsdc  OR "*-CS2025000025-#164218-27.01.2025-JT-inicio
         vbap-j_1btaxlw2 ne st_zsdt0008-j_1btaxlw2 or "st_zsdt0370-j_1btaxlw2 OR "*-CS2025000025-#164218-27.01.2025-JT-inicio
         vbap-j_1btaxlw4 ne st_zsdt0008-j_1btaxlw4 or "st_zsdt0370-j_1btaxlw4 OR "*-CS2025000025-#164218-27.01.2025-JT-inicio
         vbap-j_1btaxlw5 ne st_zsdt0008-j_1btaxlw5.   "st_zsdt0370-j_1btaxlw5.   "*-CS2025000025-#164218-27.01.2025-JT-inicio

        vl_txt01 = |Dados corretos: item { vbap-posnr }|.
        vl_txt02 = |Cod.Imp: { st_zsdt0008-j_1btxsdc } DFICMS: { vl_taxlaw }|.             "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vl_txt03 = |DFIPI: { st_zsdt0008-j_1btaxlw2 } DFCOF: { st_zsdt0008-j_1btaxlw4 } |. "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vl_txt04 = |DFPIS: { st_zsdt0008-j_1btaxlw5 }|.                                    "*-CS2025000025-#164218-27.01.2025-JT-inicio

        message e836(sd) with vl_txt01 vl_txt02 vl_txt03 vl_txt04.
      endif.
    elseif vl_taxlaw is initial.

      if vbap-j_1btxsdc   ne st_zsdt0008-j_1btxsdc  or "st_zsdt0370-j_1btxsdc  OR
         vbap-j_1btaxlw1  ne st_zsdt0008-j_1btaxlw1 or "st_zsdt0370-j_1btaxlw1 OR
         vbap-j_1btaxlw2  ne st_zsdt0008-j_1btaxlw2 or "st_zsdt0370-j_1btaxlw2 OR
         vbap-j_1btaxlw4  ne st_zsdt0008-j_1btaxlw4 or "st_zsdt0370-j_1btaxlw4 OR
         vbap-j_1btaxlw5  ne st_zsdt0008-j_1btaxlw5.   "st_zsdt0370-j_1btaxlw5.

        vl_txt01 = |Dados corretos: item { vbap-posnr }|.
        vl_txt02 = |Cod.Imp: { st_zsdt0008-j_1btxsdc } DFICMS: { st_zsdt0008-j_1btaxlw1 }|.  "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vl_txt03 = |DFIPI: { st_zsdt0008-j_1btaxlw2 } DFCOF: { st_zsdt0008-j_1btaxlw4 }|.     "*-CS2025000025-#164218-27.01.2025-JT-inicio
        vl_txt04 = |DFPIS: { st_zsdt0008-j_1btaxlw5 }|.                                        "*-CS2025000025-#164218-27.01.2025-JT-inicio
        message e836(sd) with vl_txt01 vl_txt02 vl_txt03 vl_txt04.
      endif.

    endif.
  endif.
*BBKO/Vagner Santos - Fim da alteração - 16.08.2010

  check 1 = 1.

ENDENHANCEMENT.
