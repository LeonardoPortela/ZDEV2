  DATA: vl_object TYPE thead-tdobject         ,
        vl_name   TYPE thead-tdname           ,
        vl_langu  TYPE thead-tdspras          ,
        vl_id     TYPE thead-tdid             .

  MOVE:
          'BELEG' TO vl_object,
           'S'    TO vl_langu,
          '0002'  TO vl_id   .

  CONCATENATE j_1ai02-bukrs
              j_1ai02-augbl
              j_1ai02-gjahr
         INTO vl_name.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      object          = vl_object
      name            = vl_name
      id              = vl_id
      language        = vl_langu
    TABLES
      lines           = tline
    EXCEPTIONS
      object          = 1
      id              = 2
      language        = 3
      name            = 4
      not_found       = 5
      reference_check = 6.


















