class ZCL_IM_CL_LES_ESTORNO_ADT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_CL_LES_ESTORNO_ADT
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_AC_QUANTITY_GET .
protected section.
*"* protected components of class ZCL_IM_CL_LES_ESTORNO_ADT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_CL_LES_ESTORNO_ADT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_CL_LES_ESTORNO_ADT IMPLEMENTATION.


method if_ex_ac_quantity_get~exit_quantity_change.

  field-symbols <gt_bkpf> type bkpf.

  constants: c_sapmf05a_bkpf(30) type c value '(SAPMF05A)GT_BKPF',
             c_a_confirmar              value 'A',
             c_importado                value 'I',
             cc_1_acdc                  value '1',
             cc_3_acdc                  value '3',
             c_sistema_les(3)    type c value 'LES',
             c_fb08(4)           type c value 'FB08'.

  data: ti_zlest0013    type table of zlest0013,
        ti_zlest0016    type table of zlest0016,
        ti_zlest0022    type table of zlest0022,

        lw_zlest0013    type zlest0013,
        lw_zlest0015    type zlest0015,
        lw_zlest0016    type zlest0016,
        lw_zlest0022    type zlest0022,
        lc_lote         type char10,
        ln_nrinteiro(7) type n,
        ln_decimal(3)   type n.

*----------------------------------------------------------------------
* Transação de Estorno
*----------------------------------------------------------------------
  check: sy-tcode = c_fb08 and i_ledger is initial.

*----------------------------------------------------------------------
* Documento LES
*----------------------------------------------------------------------
  assign (c_sapmf05a_bkpf) to <gt_bkpf>.
  check  <gt_bkpf> is assigned.

  if ( <gt_bkpf>-bktxt(4) = 'PF-e' ).
    call function 'Z_PFE_ESTORNO_CONTABIL'
      exporting
        bukrs = <gt_bkpf>-bukrs
        belnr = <gt_bkpf>-belnr
        gjahr = <gt_bkpf>-gjahr.
  endif.

  check <gt_bkpf>-awkey(3) = c_sistema_les
    and not <gt_bkpf>-xblnr is initial.

  lc_lote = <gt_bkpf>-xblnr. "Nr do Lote para documento LES

*----------------------------------------------------------------------
* Documento Lote com status que permite estorno
*----------------------------------------------------------------------
  select *
    into lw_zlest0015
    from zlest0015
      up to 1 rows
   where lote   = lc_lote
     and docsap = <gt_bkpf>-belnr
     and gjahr  = <gt_bkpf>-gjahr
     and bukrs  = <gt_bkpf>-bukrs
     and status = c_a_confirmar.
  endselect.
* Caso nao encontre na tabela 0015 entao pode ser documento contabil de conferencia
  if sy-subrc is not initial.
*----------------------------------------------------------------------
* Documento Conferencia que permite estorno
*----------------------------------------------------------------------
    select *
      into lw_zlest0022
      from zlest0022
     where obj_key = <gt_bkpf>-awkey.
    endselect.
    if sy-subrc is initial.
*       Altera lancamento contabil no cockpit
      update zlest0022
         set docsap = space gjahr = space obj_key = space
       where obj_key = <gt_bkpf>-awkey.
    endif.
    select count( * )
*      INTO lw_zlest0022
      from zlest0022
     where  lote eq lc_lote
      and docsap ne space.

    if sy-subrc is not initial.
*       Altera lancamento contabil no cockpit
      delete from zlest0022
       where lote = lc_lote.
*       Atualiza o lote para nova contabilização completa
      update zlest0015
         set status = 'A'
       where transportador = lw_zlest0022-transportador
         and posto         = lw_zlest0022-posto
         and lote          = lw_zlest0022-lote.
*       Atualiza o lote para nova contabilização completa
      update zlest0013
         set status = 'A'
       where codtrp    = lw_zlest0022-transportador
         and codposto  = lw_zlest0022-posto
         and lote      = lw_zlest0022-lote.
    endif.
  else.
*----------------------------------------------------------------------
* Lançamentos associados ao lote
*----------------------------------------------------------------------
    select *
      into table ti_zlest0016
      from zlest0016
     where transportador = lw_zlest0015-transportador
       and posto         = lw_zlest0015-posto
       and lote          = lw_zlest0015-lote.

    check not ti_zlest0016 is initial.

*----------------------------------------------------------------------
* Dados gerais do Lote/Lançamento envolvidos no estorno
*----------------------------------------------------------------------
    clear lw_zlest0013.

* CNPJ da transportadora
    select single stcd1
      into lw_zlest0013-cnpj_trp
      from lfa1
     where lifnr = lw_zlest0015-transportador.

* CNPJ do posto
    select single stcd1
      into lw_zlest0013-cnpj_posto
      from lfa1
     where lifnr = lw_zlest0015-posto.

    lw_zlest0013-lote     = lw_zlest0015-lote.
    lw_zlest0013-codtrp   = lw_zlest0015-transportador.
    lw_zlest0013-codposto = lw_zlest0015-posto.
    lw_zlest0013-datalote = lw_zlest0015-data.
    lw_zlest0013-vlrlote  = lw_zlest0015-vlr_importado.
    lw_zlest0013-status   = c_importado.
    lw_zlest0013-data     = sy-datum.
    lw_zlest0013-hora     = sy-uzeit.
    lw_zlest0013-usuario  = sy-uname.

    loop at ti_zlest0016 into lw_zlest0016.
      lw_zlest0013-ctafrete   = lw_zlest0016-ctafrete.
      lw_zlest0013-conhec     = lw_zlest0016-conhecimento.
      lw_zlest0013-chvid      = lw_zlest0016-chvid.
      lw_zlest0013-vlrconhec  = lw_zlest0016-vlr_importado.
      lw_zlest0013-dtacheg    = lw_zlest0016-dta_chegada.
      ln_nrinteiro = trunc( lw_zlest0016-peso_importado ).
      ln_decimal   = ( lw_zlest0016-vlr_importado - ln_nrinteiro )
                   * 1000.
      concatenate ln_nrinteiro ln_decimal into lw_zlest0013-qtde.
      append lw_zlest0013 to ti_zlest0013.
    endloop.

*----------------------------------------------------------------------
* Atualiza base de seleção de importação
*----------------------------------------------------------------------
    modify zlest0013 from table ti_zlest0013.

*----------------------------------------------------------------------
* Equaliza documento para manutenção na bade lote/lançamentos
*----------------------------------------------------------------------
    sort ti_zlest0013 by codtrp codposto lote.
    delete adjacent duplicates from ti_zlest0013
                    comparing codtrp codposto lote.

*----------------------------------------------------------------------
* Elimina entradas da base lote e lançamentos a confirmar
*----------------------------------------------------------------------
    loop at ti_zlest0013 into lw_zlest0013.
*   Elimina lote
      delete
        from zlest0015
       where transportador = lw_zlest0013-codtrp
         and posto         = lw_zlest0013-codposto
         and lote          = lw_zlest0013-lote.
*   Elimina lançamento(S) do lote
      delete
        from zlest0016
       where transportador = lw_zlest0013-codtrp
         and posto         = lw_zlest0013-codposto
         and lote          = lw_zlest0013-lote.

*   Verifica acréscimo/decréscimo aplicado
*   Não é possível restaurar o rejeitado a menos que seja por data
      select count( * )
        from zlest0022
       where transportador = lw_zlest0013-codtrp
         and posto         = lw_zlest0013-codposto
         and acdctipo      = cc_3_acdc
         and acdcinfo      = lw_zlest0013-lote.

      if sy-subrc is initial.

*     Altera acréscimo/decréscimo
        update zlest0022
           set acdctipo      = cc_1_acdc
               acdcinfo      = space
               lote_aplicado = space
         where transportador = lw_zlest0013-codtrp
           and posto         = lw_zlest0013-codposto
           and acdctipo      = cc_3_acdc
           and acdcinfo      = lw_zlest0013-lote.

      endif.

    endloop.

  endif.




endmethod.
ENDCLASS.
