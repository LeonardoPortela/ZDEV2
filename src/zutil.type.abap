type-pool zutil .


constants:
  begin of zutil_crud,
    delete type c value 'D',
    update type c value 'U',
    insert type c value 'I',
    read   type c value 'R',
  end of zutil_crud.
