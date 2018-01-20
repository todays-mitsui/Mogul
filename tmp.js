$.ajax(
  '/eval',
  {
    method: 'POST',
    data  : JSON.stringify({
      request: 'eval',
      exprStr: '` ``ski x',
    }),
  }
)
.then(data=>{ console.info(data); })
.catch(err=>{ console.info(err); });
