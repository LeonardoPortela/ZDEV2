function z_verifica_pontos_pm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VALIDA_PTO_PNEU) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_EQUIPAMENTOS STRUCTURE  EQUI
*"      T_MENSAGENS STRUCTURE  BAPIRET2 OPTIONAL
*"      T_DIIMPT STRUCTURE  DIIMPT OPTIONAL
*"----------------------------------------------------------------------

  data: tl_ret_bapi  type table of bapiret2,
        wl_return    type bapiret2,
        tl_ponto     type table of diimpt,
        tl_ponto_aux type table of diimpt,
        wl_diimpt    type diimpt,
        lv_pto_medi  type i,
        lv_tabix     type i value 0,
        tipo_eqtyp   type eqtyp,
        it_ztparam   type table of ztparam,
        rg_eqtyp     type range of eqtyp.

  free: t_diimpt,
        t_mensagens.

  field-symbols: <fs_ponto> type diimpt.
  field-symbols: <fs_equi>  type equi.


  "Seleção de parametro.
  free: rg_eqtyp, it_ztparam.
  select * from ztparam into table it_ztparam
  where param eq 'TP_PNEU'
    and const eq 'MTGE_PNEU'.
  if sy-subrc eq 0.
    rg_eqtyp = value #( for i in it_ztparam ( sign = 'I' option = 'EQ' low = i-zval ) ).
  endif.

  loop at t_equipamentos assigning <fs_equi>.

    if <fs_equi>-eqtyp in rg_eqtyp and tipo_eqtyp is not initial.
      continue.
    endif.

    if <fs_equi>-eqtyp in rg_eqtyp and tipo_eqtyp is initial.
      tipo_eqtyp = 'X'.
    endif.

    clear: lv_pto_medi, wl_return, lv_tabix.

    free: tl_ret_bapi,
          tl_ponto.

    call function 'GET_MEASURING_POINTS_4_EQUIPM'
      exporting
        i_equnr    = <fs_equi>-equnr
      tables
        et_return1 = tl_ret_bapi
        et_diimpt  = tl_ponto.

    tl_ponto_aux = tl_ponto.
    delete tl_ponto_aux where mptyp not in rg_eqtyp and mptyp ne 'M'.

    loop at tl_ponto_aux assigning <fs_ponto>.
      add 1 to lv_tabix.


**  Ignora pontos de filtro/óleo
*      IF <FS_PONTO>-MPTYP EQ 'F'
*      OR <FS_PONTO>-MPTYP EQ 'H'.
*        CLEAR: <FS_PONTO>.
*        CONTINUE.
*      ENDIF.

**  Regra para equipamento superior
      if  ( <fs_ponto>-inact ne space
            or <fs_ponto>-indtr ne space )
      and  <fs_equi>-eqtyp in rg_eqtyp.
        continue.
      endif.

**  Validar pontos duplicados
      if <fs_ponto>-atnam eq 'ODOMETRO'
      or <fs_ponto>-atnam eq 'HORIMETRO'.
        clear wl_diimpt.
        read table t_diimpt into wl_diimpt with key atnam = <fs_ponto>-atnam
                                                    equnr = <fs_equi>-equnr.
        if sy-subrc is initial.
          wl_return-type        = 'E'.
          wl_return-number      = ''.
          wl_return-message_v1  = <fs_equi>-equnr.
          shift wl_return-message_v1 left deleting leading '0'.
          wl_return-message_v2  = <fs_equi>-eqtyp.
          concatenate 'Equipamento possui duplicidade para o ponto' <fs_ponto>-atnam '.' into  wl_return-message separated by space.
          append wl_return to t_mensagens.
        else.
          if ( <fs_ponto>-indct eq space or
               <fs_ponto>-indtr eq space )
          and ( <fs_equi>-eqtyp not in rg_eqtyp ).
            add 1 to lv_pto_medi.

            wl_return-type        = 'E'.
            wl_return-number      = ''.
            wl_return-message_v1  = <fs_equi>-equnr.
            shift wl_return-message_v1 left deleting leading '0'.
            wl_return-message_v2  = <fs_equi>-eqtyp.
            wl_return-message = 'Equipamento não possui ponto de medição HORIMETRO/ODOMETRO configurado contador.'.
            append wl_return to t_mensagens.
          else.
            add 1 to lv_pto_medi .
          endif.
        endif.
      endif .

    endloop.

*IF <FS_PONTO>-MPTYP EQ 'F'
*      OR <FS_PONTO>-MPTYP EQ 'H'.
*        CONTINUE.
*      ENDIF.


    read table tl_ponto_aux into wl_diimpt with key indtr = 'X'.



*    IF <FS_PONTO>-ATNAM EQ 'ODOMETRO'
*    OR <FS_PONTO>-ATNAM EQ 'HORIMETRO'.

    if wl_diimpt-atnam eq 'ODOMETRO'
        or wl_diimpt-atnam eq 'HORIMETRO'.

      append wl_diimpt to t_diimpt.
    elseif valida_pto_pneu is initial.
      if <fs_equi>-eqtyp in rg_eqtyp.
        append wl_diimpt to t_diimpt.
      endif.
    elseif <fs_equi>-eqtyp eq 'T'.
      append wl_diimpt to t_diimpt.
    endif.

    if ( <fs_equi>-eqtyp eq 'T'  ).
      read table tl_ponto_aux into wl_diimpt with key atnam = 'PRESSAOD'.
      if ( sy-subrc is not initial ).
        wl_return-message_v1  = <fs_equi>-equnr.
        shift wl_return-message_v1 left deleting leading '0'.
        concatenate 'Pneu' wl_return-message_v1 'não possui ponto de medição PRESSAOd configurado corretamente.'
        into wl_return-message separated by space.
        append wl_return to t_mensagens.
      endif.

      read table tl_ponto_aux into wl_diimpt with key atnam = 'PRESSAOE'.
      if ( sy-subrc is not initial ).
        wl_return-message_v1  = <fs_equi>-equnr.
        shift wl_return-message_v1 left deleting leading '0'.
        concatenate 'Pneu' wl_return-message_v1 'não possui ponto de medição PRESSAOE configurado corretamente.'
        into wl_return-message separated by space.
        append wl_return to t_mensagens.
      endif.

      read table tl_ponto_aux into wl_diimpt with key atnam(4) = 'RAIA'.
      if ( sy-subrc is not initial ).
        wl_return-message_v1  = <fs_equi>-equnr.
        shift wl_return-message_v1 left deleting leading '0'.
        concatenate 'Pneu' wl_return-message_v1 'não possui ponto de medição RAIA configurado corretamente.'
        into wl_return-message separated by space.
        append wl_return to t_mensagens.
      endif.
    endif.

    unassign <fs_ponto>.

    if lv_pto_medi eq 0.

      select single * from ztparam into @data(wa_ztparam) where param = 'TP_IMPLEM' and zval eq @<fs_equi>-eqart.

      if sy-subrc is not initial.
        wl_return-type        = 'E'.
        wl_return-number      = ''.
        wl_return-message_v1  = <fs_equi>-equnr.
        shift wl_return-message_v1 left deleting leading '0'.
        wl_return-message_v2  = <fs_equi>-eqtyp.
        wl_return-message     = 'Equipamento não possui ponto de medição HORIMETRO/ODOMETRO configurado corretamente.'.
        append wl_return to t_mensagens.
      endif.
    endif.
  endloop.



  unassign <fs_equi>.

endfunction.
