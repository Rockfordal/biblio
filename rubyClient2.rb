def get_book_by_id(excon,id)
  excon.request(:method=>:get,
                :path=>"/book/#{id}",
                :headers=>{},
                :body=>nil)
end;

def get_book(excon, searchfield: nil, searchstr: nil, offset: nil, limit: nil)
  excon.request(:method=>:get,
                :path=>"/book?searchField=#{searchfield}&searchStr=#{searchstr}&offset=#{offset}&limit=#{limit}",
                :headers=>{},
                :body=>nil)
end;

def get_books(excon)
  excon.request(:method=>:get,
                :path=>"/books",
                :headers=>{},
                :body=>nil)
end;

def post_book(excon, authorization: nil, body)
  excon.request(:method=>:post,
                :path=>"/book",
                :headers=>{"Authorization"=>authorization},
                :body=>body)
end;

def put_book(excon, authorization: nil, body)
  excon.request(:method=>:put,
                :path=>"/book",
                :headers=>{"Authorization"=>authorization},
                :body=>body)
end;

def delete_book_by_id(excon,id,authorization: nil)
  excon.request(:method=>:delete,
                :path=>"/book/#{id}",
                :headers=>{"Authorization"=>authorization},
                :body=>nil)
end
