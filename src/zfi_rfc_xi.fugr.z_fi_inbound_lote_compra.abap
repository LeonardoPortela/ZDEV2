FUNCTION Z_FI_INBOUND_LOTE_COMPRA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_ZGL001_COMP_F44 STRUCTURE  ZGL001_COMP_F44
*"----------------------------------------------------------------------

data: wa_zgl0001_comp_f44 like zgl001_comp_f44,
      it_validar          like zgl001_comp_f44 occurs 0,
      vl_GJAHR            like rbkp-GJAHR,
      vl_belnr            like bkpf-belnr,
      vl_awkey            like bkpf-awkey.

loop at it_zgl001_comp_f44 into wa_zgl0001_comp_f44.

" Recuperando o ano do documento da MIRO se encontrado na tabela RBKP
"  RBKP – BELNR(Documento da miro)
  select single GJAHR
      into vl_GJAHR
      from rbkp
  where belnr = wa_zgl0001_comp_f44-belnr.
" Caso encontre o documento como um documento de material, entao recupera o doc contabil referente..
  if sy-subrc is initial.
" Procurando direto o documento contabil para um documento de MIRO
"                BKPF – AWKEY (Documento da miro + Exercicio)
    concatenate WA_ZGL0001_COMP_F44-BELNR vl_GJAHR into vl_awkey.
    SELECT SINGLE BELNR
      INTO VL_BELNR
        FROM bkpf
    where awkey = vl_awkey
      and ( blart EQ 'ZG' OR
            blart EQ 'RE' ) .

    IF SY-SUBRC IS INITIAL.
      WA_ZGL0001_COMP_F44-BELNR = VL_BELNR.
    ENDIF.
  endif.

  select *
    from zgl001_comp_f44
    into table it_validar
   where bukrs eq wa_zgl0001_comp_f44-bukrs
     and belnr eq wa_zgl0001_comp_f44-belnr
     and lote  eq wa_zgl0001_comp_f44-lote
     and buzei eq wa_zgl0001_comp_f44-buzei.

  check it_validar is initial.
  insert into  zgl001_comp_f44 values wa_zgl0001_comp_f44.

endloop.

ENDFUNCTION.
