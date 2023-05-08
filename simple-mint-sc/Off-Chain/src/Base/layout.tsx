// import Button from 'react-bootstrap/Button';
import Form from 'react-bootstrap/Form';
import { useState, useEffect, useCallback } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import MintOrBurn from './lucid';


const Layout = () => {
  
  const [amount, setAmount] : any = useState(0)

  return (
    <Form>
      <Form.Group className="mb-3" controlId="formBasicEmail">
        <Form.Label>Amount</Form.Label>
        <Form.Control type="text" placeholder="Enter amount" value={amount} onChange={(event) => setAmount(event.target.value)}/>
        <Form.Text className="text-muted">
          {amount}
        </Form.Text>
      </Form.Group>
      {/* <Button variant="primary" type="submit">
        Submit
      </Button> */}
      <MintOrBurn amount={amount} />
      
    </Form>
  );
}

export default Layout;