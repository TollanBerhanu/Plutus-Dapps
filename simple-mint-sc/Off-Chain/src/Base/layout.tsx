import Button from 'react-bootstrap/Button';
import Form from 'react-bootstrap/Form';
import { useState, useEffect, useCallback } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import { offChain } from './lucid';


const Layout = () => {
  
  const [amount, setAmount]: any = useState(0)
  const [signedTx, setSignedTx]: any = useState('')

  useEffect(() => {
    offChain(10n).then(s => {
      console.log(s)
    })
  },[])

  const getSignedTx = () => {
    console.log('Hello')
      // setSignedTx(s)
  }

  return (
    // <Form>
    //   <Form.Group className="mb-3" controlId="formBasicEmail">
    //     <Form.Label>Amount</Form.Label>
    //     <Form.Control type="text" placeholder="Enter amount"/>
    //     <Form.Text className="text-muted">
    //       {/* {amount} */}
    //     </Form.Text>
    //   </Form.Group>

    //   <Button onClick={getSignedTx} variant="primary" type="submit">
    //     Submit
    //   </Button>
    //   {/* <p>{signedTx}</p> */}
    // </Form>
    <>
      <input type='number' value={amount} onChange={(event) => setAmount(event.target.value)} />
      <p>{amount}</p>
      <button>Hello</button>
      <p> {signedTx} </p>
    </>
  );
}

export default Layout;