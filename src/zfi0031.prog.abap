*&---------------------------------------------------------------------*
*& Report ZFI0031
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi0031.

SELECT SINGLE * FROM tvarvc INTO @DATA(lv_variant)
  WHERE name = 'MAGGI_FF7AN_AJUSTESALDO'.

SELECT * FROM fqm_flow
  WHERE fi_post_date <=  '20230930'
AND deleted = ''
AND amount <> 0
AND fi_account IN ( SELECT DISTINCT valfrom FROM setleaf
WHERE 1 = 1
AND setname = 'MAGGI_FF7AN_AJUSTESALDO' )
INTO TABLE @DATA(it_fqm_flow).


LOOP AT it_fqm_flow ASSIGNING FIELD-SYMBOL(<_update>).

  IF <_update>-amount IS NOT INITIAL OR <_update>-base_amount IS NOT INITIAL.
    CLEAR: <_update>-amount, <_update>-base_amount.
    MODIFY fqm_flow FROM <_update>.
    COMMIT WORK.
  ENDIF.

ENDLOOP.
