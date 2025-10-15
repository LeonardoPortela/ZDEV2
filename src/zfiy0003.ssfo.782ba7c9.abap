DATA: stl_bkpf TYPE bkpf.
SELECT SINGLE *
FROM bkpf
INTO stl_bkpf
WHERE bukrs EQ  j_1ai02-bukrs
AND   belnr EQ  j_1ai02-augbl
AND   blart EQ 'DX'.
IF stl_bkpf-stblg IS INITIAL.
  IF copia EQ '1'.
    impresion = 'ORIGINAL'.
  ELSE.
    impresion = 'DUPLICADO'.
  ENDIF.
ELSE.
  CONCATENATE 'ANULA DOCUMENTO' stl_bkpf-stblg
INTO impresion SEPARATED BY space.
ENDIF.
















