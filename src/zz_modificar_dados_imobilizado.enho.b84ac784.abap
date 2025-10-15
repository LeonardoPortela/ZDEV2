"Name: \PR:SAPMIEQ0\FO:LEAVE_TO_TRANSACTION\SE:BEGIN\EN:ZZ_CRIAR_ORDEM_PM_IE01\SE:END\EI
ENHANCEMENT 0 ZZ_MODIFICAR_DADOS_IMOBILIZADO.
data: v_texto      type ktx01,
      v_imobiizado type anln1,
      v_desc_eqpto type anla-stadt.


if sy-ucomm eq 'BU'.
  if 'IE01_IE02_IE31_IE10' cs sy-tcode.
    if equi-eqtyp eq '1'
        or equi-eqtyp eq '2'
        or equi-eqtyp eq '3'
        or equi-eqtyp eq '4'.

      v_desc_eqpto = eqkt-eqktx.

      if iloa-anlnr is initial.
        v_imobiizado = fleet-zzimobilizado.
      else.
        v_imobiizado =   iloa-anlnr.
      endif.
      call function 'Z_TRANSFERIR_IMOBILIZADO'
        exporting
          imobilizado = v_imobiizado
          equnr       = equi-equnr "v_equipamento
          kostl       = iloa-kostl "v_KOSTL
          shtxt       = v_desc_eqpto
          bukrs       = iloa-bukrs "v_BUKRS
          gsber       = iloa-gsber "v_GSBER.
          bukrsw      = iloa-bukrs. "US 142094-08-10-2024-#142094-RJF
**********************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - FIM
**********************************************************************************************


    endif.

    case sy-tcode.
      when 'IE01'.
        message s024(sd) with |O equipamento foi criado sob o nº: { equi-equnr ALPHA = OUT }|.
      when 'IE02'.
        message s024(sd) with |O equipamento foi modificado sob o nº: { equi-equnr ALPHA = OUT }|.
      when others.
    endcase.
  endif.
endif.

ENDENHANCEMENT.
