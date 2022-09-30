(*—————————————————————————————————————————————————————————————————————
   Copyright (c) 2022-* Etienne Marais <etienne@maiste.fr>
   Distributed under the MIT license. See terms at the end of this
   file.
  ————————————————————————————————————————————————————————————————————*)

module Client = Cohttp_async.Client

type 'a io = 'a Async.Deferred.t

let return = Async.Deferred.return
let map f a = Async.Deferred.map ~f a
let bind f m = Async.Deferred.bind m ~f
let fail (`Msg m) = failwith m

(**** Helper ****)

let compute fn ~headers ~url =
  let headers = Cohttp.Header.of_list headers in
  let url = Uri.of_string url in
  Async.Deferred.bind (fn ~headers ~url) ~f:(fun (_, body) ->
      Cohttp_async.Body.to_string body)

(**** Https methods ****)

let get ~headers ~url =
  compute ~headers ~url @@ fun ~headers ~url -> Client.get ~headers url

let post ~headers ~url body =
  compute ~headers ~url @@ fun ~headers ~url ->
  let body = Cohttp_async.Body.of_string body in
  Client.post ~headers ~body url

let put ~headers ~url body =
  compute ~headers ~url @@ fun ~headers ~url ->
  let body = Cohttp_async.Body.of_string body in
  Client.put ~headers ~body url

let delete ~headers ~url =
  compute ~headers ~url @@ fun ~headers ~url -> Client.delete ~headers url

(*——————————————————————————————————————————————————————————————————————
   Copyright (c) 2022-* Etienne Marais <etienne@maiste.fr>
   Permission to use, copy, modify, and/or distribute this software for
   any purpose with or without fee is hereby granted, provided that the
   above copyright notice and this permission notice appear in all copies.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————*)
