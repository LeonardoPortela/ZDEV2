function z_saldo_adtofor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(V_LIFNR) LIKE  LFA1-LIFNR
*"     REFERENCE(I_WAERS) LIKE  EKKO-WAERS
*"     REFERENCE(I_BUKRS) LIKE  T001-BUKRS OPTIONAL
*"  EXPORTING
*"     REFERENCE(V_SALDO) TYPE  VBAK-NETWR
*"     REFERENCE(V_LIMITE) TYPE  KLIMG
*"     REFERENCE(V_DATA_VIGENCIA_INI) TYPE  DATUM
*"     REFERENCE(V_DATA_VIGENCIA_FIM) TYPE  DATUM
*"----------------------------------------------------------------------

  data: it_zfit0045 type table of zfit0045.
  "BUG SOLTO 183059
  data: r_bukrs type range of bukrs.
  data: w_bukrs like line of r_bukrs.
  "BUG SOLTO 183059

  data:
    v_konzs   type lfa1-konzs,
    lc_fator  type i,
    v_saldo_c type lfc1-umsav,
    v_saldo_a type lfc1-umsav.


  data:
    it_lfa1 type table of lfa1,
    it_bsik type table of bsik  with header line.
  select single *
     from  lfa1
     into @data(wa_lfa1)
     where lifnr = @v_lifnr.

  clear: v_saldo_c, v_saldo_a, v_limite, v_konzs.
  "BUG SOLTO 183059
  if '0001_0015_0050' cs i_bukrs.
    w_bukrs-sign   = 'I'.
    w_bukrs-option = 'EQ'.
    w_bukrs-low    = '0001'.
    append w_bukrs to r_bukrs.
    w_bukrs-low    = '0015'.
    append w_bukrs to r_bukrs.
    w_bukrs-low    = '0050'.
    append w_bukrs to r_bukrs.
  else.
    w_bukrs-sign   = 'I'.
    w_bukrs-option = 'EQ'.
    w_bukrs-low    = i_bukrs.
    append w_bukrs to r_bukrs.
  endif.
  "BUG SOLTO 183059

  if wa_lfa1-konzs is not initial. "grupo economico
    v_konzs = wa_lfa1-konzs.
    select  *
     from lfa1
     into table it_lfa1
     where konzs = v_konzs.

    select single zfit0146~lifnr
       into @data(va_lifnr)
       from zfit0146
       inner join lfa1
       on lfa1~konzs = @wa_lfa1-konzs
       and zfit0146~lifnr = lfa1~lifnr
       where zfit0146~limite > 0
       and zfit0146~bukrs in @r_bukrs ""BUG SOLTO 183059
       and zfit0146~bukrs <> @space.
    if sy-subrc eq 0.
      select sum( limite )
         into @v_limite
         from zfit0146
         inner join lfa1
         on lfa1~konzs = @wa_lfa1-konzs
         and zfit0146~lifnr = lfa1~lifnr
         where zfit0146~limite > 0
         and zfit0146~lifnr eq @va_lifnr
         and zfit0146~bukrs in @r_bukrs. ""BUG SOLTO 183059
    endif.

    clear: v_saldo_a, v_saldo_c.
                                                            "BUG183059
    loop at r_bukrs into w_bukrs.
      loop at it_lfa1 into wa_lfa1.
        refresh it_bsik.
        call function 'Z_FI_GL_PART_ABERTO'
          exporting
            i_company            = w_bukrs-low
            i_forne              = abap_true
            i_cliente            = abap_false
            i_parid              = wa_lfa1-lifnr
            i_nao_razao_especial = abap_false
          tables
            it_bsxk              = it_bsik.

        delete it_bsik where  bstat = 'S' or blart = 'VC'.
        loop at it_bsik.
          if it_bsik-shkzg eq 'H'.
            lc_fator = -1.
          else.
            lc_fator = 1.
          endif.
          "
*          if  i_waers = 'USD'.
          v_saldo_a   = it_bsik-dmbe2 * lc_fator.
*          else.
*            v_saldo_a   = it_bsik-dmbtr * lc_fator.
*          endif.
          v_saldo_c = v_saldo_c + v_saldo_a.
        endloop.
      endloop.
    endloop.
    clear v_saldo_a.
                                                            "BUG183059

  else.

    select single limite, data_vigencia_ini, data_vigencia_fim "FF #168911
     into ( @v_limite, @v_data_vigencia_ini, @v_data_vigencia_fim ) "FF #168911
    from zfit0146
   where lifnr  eq @v_lifnr
      and bukrs in @r_bukrs ""BUG SOLTO 183059 eq @i_bukrs.
      and bukrs <> @space.

    clear: v_saldo_a, v_saldo_c.
                                                            ""BUG183059
    loop at r_bukrs into w_bukrs.
      refresh it_bsik.
      call function 'Z_FI_GL_PART_ABERTO'
        exporting
          i_company            = w_bukrs-low
          i_forne              = abap_true
          i_cliente            = abap_false
          i_parid              = wa_lfa1-lifnr
          i_nao_razao_especial = abap_false
        tables
          it_bsxk              = it_bsik.

      delete it_bsik where  bstat = 'S' or blart = 'VC'.
      loop at it_bsik.
        if it_bsik-shkzg eq 'H'.
          lc_fator = -1.
        else.
          lc_fator = 1.
        endif.
        "
*        if  i_waers = 'USD'.
        v_saldo_a   = it_bsik-dmbe2 * lc_fator.
*        else.
*          v_saldo_a   = it_bsik-dmbtr * lc_fator.
*        endif.
        v_saldo_c = v_saldo_c + v_saldo_a.
      endloop.
    endloop.
    clear v_saldo_a.
                                                            "BUG183059
**************************************************************************************

  endif.


  if v_limite eq 0.
    v_saldo = 0.
  else.
    v_saldo =  v_limite -  ( v_saldo_c + v_saldo_a  ).
  endif.

endfunction.
