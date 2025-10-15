"Name: \PR:SAPLJ1BG\FO:NF_OBJECT_ADD\SE:BEGIN\EI
ENHANCEMENT 0 Z_NFE_GROUP_ITEMS.

"//Descr: Caso houver mais de um item na NF-e, verificar se trata-se de uma fatura agrupada.
"//Autor: Enio Jesus
"//Data.: 03/02/2017
"//ERP..: Leila Mara

data is_agroup_invoice type abap_bool.
data vmenge type j_1bnflin-menge.
DATA: BEGIN OF wnfstx_aux OCCURS 0.        "NF taxes per item
        INCLUDE STRUCTURE j_1bnfstx.
DATA: END OF wnfstx_aux.

if lines( wnflin ) > 0.
  data(_item_invoice) = wnflin[ 1 ].

  if lines( wnfstx[] ) > 0.
    data(_item_imposto) = wnfstx[ 1 ].
  else.
    CLEAR: _item_imposto.
  endif.

  select single *
    from zsdt0121
    into @data(_parameter)
   where werks = @_item_invoice-werks
     and matnr = @_item_invoice-matnr.

 IF ( sy-subrc is initial ).

  LOOP AT wnflin[] into wnflin.
    if wnflin-matnr EQ _parameter-matnr.
      is_agroup_invoice = abap_true.
    else.
      is_agroup_invoice = abap_false.
      exit.
    endif.
  ENDLOOP.

  IF ( is_agroup_invoice = abap_true ).
    clear vmenge.
    LOOP AT wnflin.
      add wnflin-menge to vmenge.
    ENDLOOP.
    clear wnflin[].
    wnfdoc-brgew = vmenge.
    wnfdoc-ntgew = vmenge.

    CLEAR: wnfdoc-ANZPK.

    _item_invoice-menge      = wnfdoc-brgew.
    _item_invoice-netwr      = wnfdoc-nftot.
    _item_invoice-netwrt     = wnfdoc-nftot.
    _item_invoice-nfnet      = wnfdoc-nftot.
    _item_invoice-menge_trib = wnfdoc-brgew.
    _item_invoice-nfnett     = wnfdoc-nftot.
    append _item_invoice to wnflin.
    "
    wnfstx_aux[] = wnfstx[].
    clear wnfstx[].
    LOOP AT wnfstx_aux.
      MOVE-CORRESPONDING wnfstx_aux to _item_imposto.
      _item_imposto-ITMNUM = _item_invoice-ITMNUM.
      COLLECT _item_imposto into wnfstx.
    ENDLOOP.

  ENDIF.
 ENDIF.
endif.
ENDENHANCEMENT.
