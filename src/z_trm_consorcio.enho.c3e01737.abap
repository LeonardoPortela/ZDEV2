"Name: \TY:CL_FTR_CASH_FLOW_CALCULATOR\IN:IF_FTR_CASH_FLOW_CALCULATOR\ME:CALCULATE_FLOWS\SE:END\EI
ENHANCEMENT 0 Z_TRM_CONSORCIO.

  SELECT COUNT(*)
    FROM tvarvc
    WHERE name = 'Z_TRM_CALC_Z'
      AND low = abap_true.

  IF sy-subrc = 0.

    DATA(lo_calc) = NEW zcltrm_calculo_fluxo(
      it_fluxo = it_flows
      it_cond  = it_conditions  ).

    lo_calc->modifica_fluxo(
      CHANGING
        ct_fluxo = et_flows   ).

  ENDIF.

  IF 1 = 2.

    DATA et_flows_aux TYPE ftr_fhapo.
    DATA wcont         TYPE i.
    READ TABLE et_flows ASSIGNING FIELD-SYMBOL(<fs_flows>) WITH KEY sfhazba = '1106'.
    IF sy-subrc = 0.
      et_flows_aux[] = et_flows[].
      DELETE et_flows_aux WHERE sfhazba NE '1130'.
      DESCRIBE TABLE et_flows_aux LINES wcont.
      IF wcont GT 0.
        LOOP AT et_flows ASSIGNING FIELD-SYMBOL(<fs_flows2>) WHERE sfhazba EQ '1203' OR sfhazba EQ '1201'.
          <fs_flows2>-bzbetr = ( <fs_flows>-bzbetr / wcont ) * ( <fs_flows2>-pkond / 10 ).
          <fs_flows2>-bnwhr = <fs_flows2>-bzbetr.
          <fs_flows2>-bbasis = <fs_flows>-bzbetr.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
