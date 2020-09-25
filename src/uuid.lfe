(defmodule uuid
    (export (v3 2) (v4 0) (v5 2))
    (export (to-string 1) (to-binary 1))
    (export (version 1) (is-v3 1) (is-v4 1) (is-v5 1))
    (export all))


(defun v4 []
    (compose-uuid 4 2 (crypto:strong_rand_bytes 16)))

(defun v3
    "Create a UUID v3 (name based, MD5 is hashing function) as a binary. Magic numbers are from Appendix C of the RFC 4122."
    (['dns Name] (when (is_list Name)) (create-namebased-uuid 'md5 (list_to_binary (list (binary (#x6ba7b8109dad11d180b400c04fd430c8 (size 128))) Name))))
    (['url Name] (when (is_list Name)) (create-namebased-uuid 'md5 (list_to_binary (list (binary (#x6ba7b8119dad11d180b400c04fd430c8 (size 128))) Name))))
    (['oid Name] (when (is_list Name)) (create-namebased-uuid 'md5 (list_to_binary (list (binary (#x6ba7b8129dad11d180b400c04fd430c8 (size 128))) Name))))
    (['x500 Name] (when (is_list Name)) (create-namebased-uuid 'md5 (list_to_binary (list (binary (#x6ba7b8149dad11d180b400c04fd430c8 (size 128))) Name))))
    (['nil Name] (when (is_list Name)) (create-namebased-uuid 'md5 (list_to_binary (list (binary (#x0 (size 128))) Name))))
    ([UuidStr Name] (when (and (is_list UuidStr) (is_list Name))) (create-namebased-uuid 'md5 (list_to_binary (list (to-binary UuidStr) Name))))
    ([UuidBin Name] (when (and (is_binary UuidBin) (is_list Name))) (create-namebased-uuid 'md5 (list_to_binary (list UuidBin Name))))
    )


(defun v5
    "Create a UUID v5 (name based, SHA1 is hashing function) as a binary. Magic numbers are from Appendix C of the RFC 4122."
    (['dns Name]  (when (is_list Name)) (create-namebased-uuid 'sha (list_to_binary (list (binary (#x6ba7b8109dad11d180b400c04fd430c8 (size 128))) Name))))
    (['url Name]  (when (is_list Name)) (create-namebased-uuid 'sha (list_to_binary (list (binary (#x6ba7b8119dad11d180b400c04fd430c8 (size 128))) Name))))
    (['oid Name]  (when (is_list Name)) (create-namebased-uuid 'sha (list_to_binary (list (binary (#x6ba7b8129dad11d180b400c04fd430c8 (size 128))) Name))))
    (['x500 Name] (when (is_list Name)) (create-namebased-uuid 'sha (list_to_binary (list (binary (#x6ba7b8149dad11d180b400c04fd430c8 (size 128))) Name))))
    (['nil Name] (when (is_list Name)) (create-namebased-uuid 'sha (list_to_binary (list (binary (#x0 (size 128))) Name))))
    ([UuidStr Name] (when (and (is_list UuidStr) (is_list Name))) (create-namebased-uuid 'sha (list_to_binary (list (to-binary UuidStr) Name))))
    ([UuidBin Name] (when (and (is_binary UuidBin) (is_list Name))) (create-namebased-uuid 'sha (list_to_binary (list UuidBin Name))))
    )

(defun create-namebased-uuid
    (['md5 Data] (when) (compose-uuid 3 2 (crypto:hash 'md5 Data)))
    (['sha Data] (when) (compose-uuid 5 2 (binary:part (crypto:hash 'sha Data) 0 16))))

(defun compose-uuid
    "Compose UUID with input (hashed/random) data."
    ([Version Variant (binary (u0 (size 32)) (u1 (size 16)) (_ (size 4)) (u2 (size 12)) (_ (size 2)) (u3 (size 30)) (u4 (size 32)))]
        (binary (u0 (size 32)) (u1 (size 16)) (Version (size 4)) (u2 (size 12)) (Variant (size 2)) (u3 (size 30)) (u4 (size 32)))))

(defun to-string [U]
    (lists:flatten 
        (io_lib:format "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b" (get-parts U))))

(defun get-parts
    ([(binary (TL (size 32)) (TM (size 16)) (THV (size 16)) (CSR (size 8)) (CSL (size 8)) (N (size 48)))] 
        (list TL TM THV CSR CSL N)))

(defun hex-to-int [Hex]
    (let (((tuple 'ok (list D) '()) (io_lib:fread "~16u" Hex)))
        D))

(defun to-binary
    ([UuidStr] (when (is_list UuidStr))
        (case (length UuidStr)
            (32 (when) (to-binary 'simple UuidStr))
            (36 (when) (to-binary 'pretty UuidStr))))
    ([_] (when) (erlang:error 'badarg)))

(defun to-binary
    (['simple UuidStr] (when) 
        (binary ((hex-to-int UuidStr) (size 128))))
    (['pretty UuidStr] (when) 
        (let ((P (lists:map #'hex-to-int/1 (string:tokens UuidStr "$-")))) 
            (binary 
                ((lists:nth 1 P) (size 32)) ((lists:nth 2 P) (size 16)) ((lists:nth 3 P) (size 16)) 
                ((lists:nth 4 P) (size 16)) ((lists:nth 5 P) (size 48))))))


(defun version
    ([(binary (_ (size 48)) (Version (size 4)) (_ (size 76)))] (when) Version)
    ([UuidStr] (when (is_list UuidStr)) (version (to-binary UuidStr)))
    ([_] (when) (erlang:error 'badarg)))


(defun is-v3 [Uuid]
    (== (version Uuid) 3))

(defun is-v4 [Uuid]
    (== (version Uuid) 4))

(defun is-v5 [Uuid]
    (== (version Uuid) 5))